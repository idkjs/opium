open Import;

let log_src = Logs.Src.create("opium.server");

module Log = (val Logs.src_log(log_src): Logs.LOG);

let body_to_string = (~content_type="text/plain", ~max_len=1000, body) => {
  open Lwt.Syntax;
  let (lhs, rhs) =
    switch (String.split_on_char(~sep='/', content_type)) {
    | [lhs, rhs] => (lhs, rhs)
    | _ => ("application", "octet-stream")
    };

  switch (lhs, rhs) {
  | ("text", _)
  | ("application", "json")
  | ("application", "x-www-form-urlencoded") =>
    let+ s = Body.copy(body) |> Body.to_string;
    if (String.length(s) > max_len) {
      String.sub(s, ~pos=0, ~len=min(String.length(s), max_len))
      ++ Format.asprintf(
           " [truncated %d characters]",
           String.length(s) - max_len,
         );
    } else {
      s;
    };
  | _ => Lwt.return("<" ++ content_type ++ ">")
  };
};

let request_to_string = (request: Request.t) => {
  open Lwt.Syntax;
  let content_type = Request.content_type(request);
  let+ body_string = body_to_string(~content_type?, request.body);
  Format.asprintf(
    "%s %s %s\n%s\n\n%s\n%!",
    Method.to_string(request.meth),
    request.target,
    Version.to_string(request.version),
    Headers.to_string(request.headers),
    body_string,
  );
};

let response_to_string = (response: Response.t) => {
  open Lwt.Syntax;
  let content_type = Response.content_type(response);
  let+ body_string = body_to_string(~content_type?, response.body);
  Format.asprintf(
    "%a %a %s\n%a\n%s\n%!",
    Version.pp_hum,
    response.version,
    Status.pp_hum,
    response.status,
    Option.value(~default="", response.reason),
    Headers.pp_hum,
    response.headers,
    body_string,
  );
};

let respond = (handler, req) => {
  let time_f = f => {
    let t1 = Mtime_clock.now();
    let x = f();
    let t2 = Mtime_clock.now();
    let span = Mtime.span(t1, t2);
    (span, x);
  };

  open Lwt.Syntax;
  let f = () => handler(req);
  let (span, response_lwt) = time_f(f);
  let* response = response_lwt;
  let code = response.Response.status |> Status.to_string;
  Log.info(m =>
    m("Responded with HTTP code %s in %a", code, Mtime.Span.pp, span)
  );
  let+ response_string = response_to_string(response);
  Log.debug(m => m("%s", response_string));
  response;
};

let m = {
  open Lwt.Syntax;
  let filter = (handler, req) => {
    let meth = Method.to_string(req.Request.meth);
    let uri = req.Request.target |> Uri.of_string |> Uri.path_and_query;
    Logs.info(~src=log_src, m => m("Received %s %S", meth, uri));
    let* request_string = request_to_string(req);
    Logs.debug(~src=log_src, m => m("%s", request_string));
    Lwt.catch(
      () => respond(handler, req),
      exn => {
        Logs.err(~src=log_src, f => f("%s", Nifty.Exn.to_string(exn)));
        Lwt.fail(exn);
      },
    );
  };

  Rock.Middleware.create(~name="Logger", ~filter);
};
