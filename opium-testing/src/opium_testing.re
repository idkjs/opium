module Testable = {
  let status = Alcotest.of_pp(Opium.Status.pp);
  let meth = Alcotest.of_pp(Opium.Method.pp);
  let version = Alcotest.of_pp(Opium.Version.pp);
  let body = Alcotest.of_pp(Opium.Body.pp);
  let request = Alcotest.of_pp(Opium.Request.pp);
  let response = Alcotest.of_pp(Opium.Response.pp);
  let cookie = Alcotest.of_pp(Opium.Cookie.pp);
};

let handle_request = app => {
  open Lwt.Syntax;
  let service = Opium.App.to_handler(app);
  let request_handler = request => {
    let+ {Opium.Response.body, headers, _} as response = service(request);
    let length = Opium.Body.length(body);
    let headers =
      switch (length) {
      | None =>
        Opium.Headers.add_unless_exists(
          headers,
          "Transfer-Encoding",
          "chunked",
        )
      | Some(l) =>
        Opium.Headers.add_unless_exists(
          headers,
          "Content-Length",
          Int64.to_string(l),
        )
      };

    {...response, headers};
  };

  request_handler;
};

let check_status = (~msg=?, expected, t) => {
  let message =
    switch (msg) {
    | Some(msg) => msg
    | None =>
      Format.asprintf("HTTP status is %d", Opium.Status.to_code(expected))
    };

  Alcotest.check(Testable.status, message, expected, t);
};

let check_status' = (~msg=?, ~expected, ~actual) =>
  check_status(~msg?, expected, actual);

let check_meth = (~msg=?, expected, t) => {
  let message =
    switch (msg) {
    | Some(msg) => msg
    | None =>
      Format.asprintf("HTTP method is %s", Opium.Method.to_string(expected))
    };

  Alcotest.check(Testable.meth, message, expected, t);
};

let check_meth' = (~msg=?, ~expected, ~actual) =>
  check_meth(~msg?, expected, actual);

let check_version = (~msg=?, expected, t) => {
  let message =
    switch (msg) {
    | Some(msg) => msg
    | None =>
      Format.asprintf(
        "HTTP version is %s",
        Opium.Version.to_string(expected),
      )
    };

  Alcotest.check(Testable.version, message, expected, t);
};

let check_version' = (~msg=?, ~expected, ~actual) =>
  check_version(~msg?, expected, actual);

let check_body = (~msg=?, expected, t) => {
  let message =
    switch (msg) {
    | Some(msg) => msg
    | None => "bodies are equal"
    };

  Alcotest.check(Testable.body, message, expected, t);
};

let check_body' = (~msg=?, ~expected, ~actual) =>
  check_body(~msg?, expected, actual);

let check_request = (~msg=?, expected, t) => {
  let message =
    switch (msg) {
    | Some(msg) => msg
    | None => "requests are equal"
    };

  Alcotest.check(Testable.request, message, expected, t);
};

let check_request' = (~msg=?, ~expected, ~actual) =>
  check_request(~msg?, expected, actual);

let check_response = (~msg=?, expected, t) => {
  let message =
    switch (msg) {
    | Some(msg) => msg
    | None => "responses are equal"
    };

  Alcotest.check(Testable.response, message, expected, t);
};

let check_response' = (~msg=?, ~expected, ~actual) =>
  check_response(~msg?, expected, actual);

let string_contains = (s1, s2) => {
  let re = Str.regexp_string(s2);
  try(
    {
      ignore(Str.search_forward(re, s1, 0));
      true;
    }
  ) {
  | Not_found => false
  };
};

let check_body_contains = (~msg=?, s, body) => {
  let message =
    switch (msg) {
    | Some(msg) => msg
    | None => "response body contains" ++ s
    };

  open Lwt.Syntax;
  let+ body = body |> Opium.Body.copy |> Opium.Body.to_string;
  Alcotest.check(Alcotest.bool, message, true, string_contains(body, s));
};

let check_cookie = (~msg=?, expected, t) => {
  let message =
    switch (msg) {
    | Some(msg) => msg
    | None => "cookies are equal"
    };

  Alcotest.check(Testable.cookie, message, expected, t);
};

let check_cookie' = (~msg=?, ~expected, ~actual) =>
  check_cookie(~msg?, expected, actual);
