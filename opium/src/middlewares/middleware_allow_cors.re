open Import;

let default_origin = ["*"];
let default_credentials = true;
let default_max_age = 1_728_000;

let default_headers = [
  "Authorization",
  "Content-Type",
  "Accept",
  "Origin",
  "User-Agent",
  "DNT",
  "Cache-Control",
  "X-Mx-ReqToken",
  "Keep-Alive",
  "X-Requested-With",
  "If-Modified-Since",
  "X-CSRF-Token",
];

let default_expose = [];
let default_methods = [
  `GET,
  `POST,
  `PUT,
  `DELETE,
  `OPTIONS,
  `Other("PATCH"),
];
let default_send_preflight_response = true;
let request_origin = request => Request.header("Origin", request);

let request_vary = request =>
  switch (Request.header("Vary", request)) {
  | None => []
  | Some(s) => String.split_on_char(~sep=',', s)
  };

let allowed_origin = (origins, request) => {
  let request_origin = request_origin(request);
  switch (request_origin) {
  | Some(request_origin)
      when List.exists(~f=String.equal(request_origin), origins) =>
    Some(request_origin)
  | _ =>
    if (List.exists(~f=String.equal("*"), origins)) {
      Some("*");
    } else {
      None;
    }
  };
};

let vary_headers = (allowed_origin, hs) => {
  let vary_header = request_vary(hs);
  switch (allowed_origin, vary_header) {
  | (Some("*"), _) => []
  | (None, _) => []
  | (_, []) => [("Vary", "Origin")]
  | (_, headers) => [
      ("Vary", ["Origin", ...headers] |> String.concat(~sep=",")),
    ]
  };
};

let cors_headers = (~origins, ~credentials, ~expose, request) => {
  let allowed_origin = allowed_origin(origins, request);
  let vary_headers = vary_headers(allowed_origin, request);
  [
    (
      "Access-Control-Allow-Origin",
      allowed_origin |> Option.value(~default=""),
    ),
    ("Access-Control-Expose-Headers", String.concat(~sep=",", expose)),
    ("Access-Control-Allow-Credentials", Bool.to_string(credentials)),
  ]
  @ vary_headers;
};

let allowed_headers = (~headers, request) => {
  let value =
    switch (headers) {
    | ["*"] =>
      Request.header("Access-Control-Request-Headers", request)
      |> Option.value(~default="")
    | headers => String.concat(~sep=",", headers)
    };

  [("Access-Control-Allow-Headers", value)];
};

let options_cors_headers = (~max_age, ~headers, ~methods, request) => {
  let methods = List.map(methods, ~f=Method.to_string);
  [
    ("Access-Control-Max-Age", string_of_int(max_age)),
    ("Access-Control-Allow-Methods", String.concat(~sep=",", methods)),
  ]
  @ allowed_headers(~headers, request);
};

let m =
    (
      ~origins=default_origin,
      ~credentials=default_credentials,
      ~max_age=default_max_age,
      ~headers=default_headers,
      ~expose=default_expose,
      ~methods=default_methods,
      ~send_preflight_response=default_send_preflight_response,
      (),
    ) => {
  open Lwt.Syntax;
  let filter = (handler, req) => {
    let+ response = handler(req);
    let hs = cors_headers(~origins, ~credentials, ~expose, req);
    let hs =
      if (req.Request.meth == `OPTIONS) {
        hs @ options_cors_headers(~max_age, ~headers, ~methods, req);
      } else {
        hs;
      };

    switch (send_preflight_response, req.Request.meth) {
    | (true, `OPTIONS) =>
      Response.make(~status=`No_content, ~headers=Headers.of_list(hs), ())
    | _ => {
        ...response,
        headers: Headers.add_list(response.Response.headers, hs |> List.rev),
      }
    };
  };

  Rock.Middleware.create(~name="Allow CORS", ~filter);
};
