open Opium;

let request = Alcotest.of_pp(Request.pp_hum);
let response = Alcotest.of_pp(Response.pp_hum);

let with_service = (~middlewares=?, ~handler=?, f) => {
  let handler =
    Option.value(handler, ~default=_req => Lwt.return @@ Response.make());

  let middlewares = Option.value(middlewares, ~default=[]);
  let app = Rock.App.create(~middlewares, ~handler, ());
  let {Rock.App.middlewares, handler} = app;
  let filters = ListLabels.map(~f=m => m.Rock.Middleware.filter, middlewares);
  let service = Rock.Filter.apply_all(filters, handler);
  f(service);
};

let check_response = (~headers=?, ~status=?, res) => {
  let expected =
    Response.make(
      ~status?,
      ~headers=?Option.map(Headers.of_list, headers),
      (),
    );
  Alcotest.(check(response))("same response", expected, res);
};

let test_regular_request = () => {
  open Lwt.Syntax;
  let+ res =
    with_service(
      ~middlewares=[Middleware.allow_cors()],
      service => {
        let req = Request.make("/", `GET);
        service(req);
      },
    );
  check_response(
    ~headers=[
      ("access-control-allow-origin", "*"),
      ("access-control-expose-headers", ""),
      ("access-control-allow-credentials", "true"),
    ],
    res,
  );
};

let test_overwrite_origin = () => {
  open Lwt.Syntax;
  let+ res =
    with_service(
      ~middlewares=[
        Middleware.allow_cors(~origins=["http://example.com"], ()),
      ],
      service => {
        let req =
          Request.make(
            "/",
            `GET,
            ~headers=Headers.of_list([("origin", "http://example.com")]),
          );

        service(req);
      },
    );
  check_response(
    ~headers=[
      ("access-control-allow-origin", "http://example.com"),
      ("access-control-expose-headers", ""),
      ("access-control-allow-credentials", "true"),
      ("vary", "Origin"),
    ],
    res,
  );
};

let test_return_204_for_options = () => {
  open Lwt.Syntax;
  let+ res =
    with_service(
      ~middlewares=[Middleware.allow_cors()],
      service => {
        let req = Request.make("/", `OPTIONS);
        service(req);
      },
    );
  check_response(
    ~status=`No_content,
    ~headers=[
      ("access-control-allow-origin", "*"),
      ("access-control-expose-headers", ""),
      ("access-control-allow-credentials", "true"),
      ("access-control-max-age", "1728000"),
      ("access-control-allow-methods", "GET,POST,PUT,DELETE,OPTIONS,PATCH"),
      (
        "access-control-allow-headers",
        "Authorization,Content-Type,Accept,Origin,User-Agent,DNT,Cache-Control,X-Mx-ReqToken,Keep-Alive,X-Requested-With,If-Modified-Since,X-CSRF-Token",
      ),
    ],
    res,
  );
};

let test_allow_request_headers = () => {
  open Lwt.Syntax;
  let+ res =
    with_service(
      ~middlewares=[Middleware.allow_cors(~headers=["*"], ())],
      service => {
        let req =
          Request.make(
            "/",
            `OPTIONS,
            ~headers=
              Headers.of_list([
                ("access-control-request-headers", "header-1,header-2"),
              ]),
          );

        service(req);
      },
    );
  check_response(
    ~status=`No_content,
    ~headers=[
      ("access-control-allow-origin", "*"),
      ("access-control-expose-headers", ""),
      ("access-control-allow-credentials", "true"),
      ("access-control-max-age", "1728000"),
      ("access-control-allow-methods", "GET,POST,PUT,DELETE,OPTIONS,PATCH"),
      ("access-control-allow-headers", "header-1,header-2"),
    ],
    res,
  );
};

let () =
  Lwt_main.run @@
  Alcotest_lwt.run(
    "Middleware :: Allow CORS",
    [
      (
        "headers",
        [
          (
            "Regular request returns correct headers",
            `Quick,
            test_regular_request,
          ),
          ("Overwrites origin header", `Quick, test_overwrite_origin),
          (
            "Allow incoming request headers",
            `Quick,
            test_allow_request_headers,
          ),
          (
            "Returns No Content for OPTIONS requests",
            `Quick,
            test_return_204_for_options,
          ),
        ],
      ),
    ],
  );
