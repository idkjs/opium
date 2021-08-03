open Import;
module Server = Httpaf_lwt_unix.Server;
module Reqd = Httpaf.Reqd;
open Lwt.Syntax;
let (let*) = Lwt.bind;
let err_invalid_host = host =>
  Lwt.fail_invalid_arg("Could not get host info for `" ++ host ++ "`");

let make_connection_handler = (~host, ~port, ~middlewares=?, handler) => {
  let* host_entry =
    Lwt.catch(
      () => Lwt_unix.gethostbyname(host),
      fun
      | Not_found => err_invalid_host(host)
      | exn => Lwt.fail(exn),
    );
  let inet_addr = host_entry.h_addr_list[0];
  let listen_address = [@implicit_arity] Unix.ADDR_INET(inet_addr, port);
  let connection_handler = (addr, fd) => {
    let f = (~request_handler, ~error_handler) =>
      Httpaf_lwt_unix.Server.create_connection_handler(
        ~request_handler=_ => request_handler,
        ~error_handler=_ => error_handler,
        addr,
        fd,
      );

    let app = Rock.App.create(~middlewares?, ~handler, ());
    Rock.Server_connection.run(f, app);
  };

  Lwt.return((listen_address, connection_handler));
};

let run_unix = (~backlog=?, ~middlewares=?, ~host, ~port, handler) => {
  let* (listen_address, connection_handler) =
    make_connection_handler(~middlewares?, ~host, ~port, handler);
  Lwt_io.establish_server_with_client_socket(
    ~backlog?,
    listen_address,
    connection_handler,
  );
};

let run_unix_multicore = (~middlewares=?, ~host, ~port, ~jobs, handler) => {
  let (listen_address, connection_handler) =
    Lwt_main.run @@
    make_connection_handler(~middlewares?, ~host, ~port, handler);

  let socket =
    Lwt_unix.socket(
      Unix.domain_of_sockaddr(listen_address),
      Unix.SOCK_STREAM,
      0,
    );

  Lwt_unix.setsockopt(socket, Unix.SO_REUSEADDR, true);
  Lwt_main.run(
    {
      let+ () = Lwt_unix.bind(socket, listen_address);
      Lwt_unix.listen(socket, [@ocaml.warning "-3"] Lwt_unix.somaxconn());
    },
  );
  let rec accept_loop = (socket, instance) => {
    let* (socket', sockaddr') = Lwt_unix.accept(socket);
    Lwt.async(() => connection_handler(sockaddr', socket'));
    accept_loop(socket, instance);
  };

  for (i in 1 to jobs) {
    flush_all();
    if (Lwt_unix.fork() == 0) {
      Lwt.async(() => accept_loop(socket, i));
      let (forever, _) = Lwt.wait();
      Lwt_main.run(forever);
      exit(0);
    };
  };
  while (true) {
    Unix.pause();
  };
};

type t = {
  host: string,
  port: int,
  jobs: int,
  backlog: option(int),
  debug: bool,
  verbose: bool,
  routes: list((Httpaf.Method.t, Route.t, Rock.Handler.t)),
  middlewares: list(Rock.Middleware.t),
  name: string,
  not_found: Rock.Handler.t,
};

type builder = t => t;
type route = (string, Rock.Handler.t) => builder;

let register = (app, ~meth, ~route, ~action) => {
  ...app,
  routes: [(meth, route, action), ...app.routes],
};

let default_not_found = _ =>
  Lwt.return(
    Response.make(
      ~status=`Not_found,
      ~body=
        Body.of_string("<html><body><h1>404 - Not found</h1></body></html>"),
      (),
    ),
  );

let system_cores =
  switch (Sys.unix) {
  | false =>
    /* TODO: detect number of cores on Windows */
    1
  | true =>
    let ic = Unix.open_process_in("getconf _NPROCESSORS_ONLN");
    let cores = int_of_string(input_line(ic));
    ignore(Unix.close_process_in(ic));
    cores;
  };

let empty = {
  name: "Opium Default Name",
  host: "0.0.0.0",
  port: 3000,
  jobs: system_cores,
  backlog: None,
  debug: false,
  verbose: false,
  routes: [],
  middlewares: [],
  not_found: default_not_found,
};

let create_router = routes =>
  List.fold_left(
    routes, ~init=Middleware_router.empty, ~f=(router, (meth, route, action)) =>
    Middleware_router.add(router, ~meth, ~route, ~action)
  );

let attach_middleware = ({verbose, debug, routes, middlewares, _}) =>
  [Some(routes |> create_router |> Middleware_router.m)]
  @ List.map(~f=Option.some, middlewares)
  @ [
    if (verbose) {
      Some(Middleware_logger.m);
    } else {
      None;
    },
    if (debug) {
      Some(Middleware_debugger.m);
    } else {
      None;
    },
  ]
  |> List.filter_opt;

let to_handler = app => {
  let middlewares = attach_middleware(app);
  let filters = List.map(~f=m => m.Rock.Middleware.filter, middlewares);
  let service = Rock.Filter.apply_all(filters, app.not_found);
  service;
};

let port = (port, t) => {...t, port};
let jobs = (jobs, t) => {...t, jobs};
let backlog = (backlog, t) => {...t, backlog: Some(backlog)};
let host = (host, t) => {...t, host};
let cmd_name = (name, t) => {...t, name};
let middleware = (m, app) => {...app, middlewares: [m, ...app.middlewares]};
let action = (meth, route, action) =>
  register(~meth, ~route=Route.of_string(route), ~action);

let not_found = (action, t) => {
  let action = req => {
    let+ (headers, body) = action(req);
    Response.make(~headers, ~body, ~status=`Not_found, ());
  };

  {...t, not_found: action};
};

let get = (route, action) =>
  register(~meth=`GET, ~route=Route.of_string(route), ~action);
let post = (route, action) =>
  register(~meth=`POST, ~route=Route.of_string(route), ~action);
let delete = (route, action) =>
  register(~meth=`DELETE, ~route=Route.of_string(route), ~action);
let put = (route, action) =>
  register(~meth=`PUT, ~route=Route.of_string(route), ~action);

let patch = (route, action) =>
  register(~meth=`Other("PATCH"), ~route=Route.of_string(route), ~action);

let head = (route, action) =>
  register(~meth=`HEAD, ~route=Route.of_string(route), ~action);
let options = (route, action) =>
  register(~meth=`OPTIONS, ~route=Route.of_string(route), ~action);

let any = (methods, route, action, t) => {
  if (List.length(methods) == 0) {
    Logs.warn(f =>
      f(
        "Warning: you're using [any] attempting to bind to '%s' but your list\n        of http methods is empty route",
        route,
      )
    );
  };
  let route = Route.of_string(route);
  methods
  |> List.fold_left(~init=t, ~f=(app, meth) =>
       app |> register(~meth, ~route, ~action)
     );
};

let all = any([`GET, `POST, `DELETE, `PUT, `HEAD, `OPTIONS]);

let setup_logger = app => {
  if (app.verbose) {
    Logs.set_reporter(Logs_fmt.reporter());
    Logs.set_level(Some(Logs.Info));
  };
  if (app.debug) {
    Logs.set_level(Some(Logs.Debug));
  };
};

let start = app => {
  /* We initialize the middlewares first, because the logger middleware initializes the
     logger. */
  let middlewares = attach_middleware(app);
  setup_logger(app);
  Logs.info(f =>
    f(
      "Starting Opium on %s:%d%s",
      app.host,
      app.port,
      if (app.debug) {
        " (debug mode)";
      } else {
        "";
      },
    )
  );
  run_unix(
    ~backlog=?app.backlog,
    ~middlewares,
    ~host=app.host,
    ~port=app.port,
    app.not_found,
  );
};

let start_multicore = app => {
  /* We initialize the middlewares first, because the logger middleware initializes the
     logger. */
  let middlewares = attach_middleware(app);
  setup_logger(app);
  Logs.info(f =>
    f(
      "Starting Opium on %s:%d with %d cores%s",
      app.host,
      app.port,
      app.jobs,
      if (app.debug) {
        " (debug mode)";
      } else {
        "";
      },
    )
  );
  run_unix_multicore(
    ~middlewares,
    ~host=app.host,
    ~port=app.port,
    ~jobs=app.jobs,
    app.not_found,
  );
};

let hashtbl_add_multi = (tbl, x, y) => {
  let l =
    try(Hashtbl.find(tbl, x)) {
    | Not_found => []
    };

  Hashtbl.replace(tbl, x, [y, ...l]);
};

let print_routes_f = routes => {
  let routes_tbl = Hashtbl.create(64);
  routes
  |> List.iter(~f=((meth, route, _)) =>
       hashtbl_add_multi(routes_tbl, route, meth)
     );
  Printf.printf("%d Routes:\n", Hashtbl.length(routes_tbl));
  Hashtbl.iter(
    (key, data) =>
      Printf.printf(
        "> %s (%s)\n",
        Route.to_string(key),
        data
        |> List.map(~f=m =>
             Httpaf.Method.to_string(m) |> String.uppercase_ascii
           )
        |> String.concat(~sep=" "),
      ),
    routes_tbl,
  );
};

let print_middleware_f = middlewares => {
  print_endline("Active middleware:");
  middlewares
  |> List.map(~f=m => m.Rock.Middleware.name)
  |> List.iter(~f=Printf.printf("> %s \n"));
};

let setup_app =
    (
      app,
      port,
      jobs,
      host,
      print_routes,
      print_middleware,
      debug,
      verbose,
      _errors,
    ) => {
  let app = {...app, debug, verbose, host, port, jobs};
  if (print_routes) {
    let routes = app.routes;
    print_routes_f(routes);
    exit(0);
  };
  if (print_middleware) {
    let middlewares = app.middlewares;
    print_middleware_f(middlewares);
    exit(0);
  };
  app;
};

module Cmds = {
  open Cmdliner;

  let routes = {
    let doc = "print routes";
    Arg.(value & flag & info(["r", "routes"], ~doc));
  };

  let middleware = {
    let doc = "print middleware stack";
    Arg.(value & flag & info(["m", "middlware"], ~doc));
  };

  let port = default => {
    let doc = "port";
    Arg.(value & opt(int, default) & info(["p", "port"], ~doc));
  };

  let jobs = default => {
    let doc = "jobs";
    Arg.(value & opt(int, default) & info(["j", "jobs"], ~doc));
  };

  let host = default => {
    let doc = "host";
    Arg.(value & opt(string, default) & info(["h", "host"], ~doc));
  };

  let debug = {
    let doc = "enable debug information";
    Arg.(value & flag & info(["d", "debug"], ~doc));
  };

  let verbose = {
    let doc = "enable verbose mode";
    Arg.(value & flag & info(["v", "verbose"], ~doc));
  };

  let errors = {
    let doc = "raise on errors. default is print";
    Arg.(value & flag & info(["f", "fatal"], ~doc));
  };

  let term =
    Cmdliner.Term.(
      app =>
        pure(setup_app)
        $ pure(app)
        $ port(app.port)
        $ jobs(app.jobs)
        $ host(app.host)
        $ routes
        $ middleware
        $ debug
        $ verbose
        $ errors
    );

  let info = name => {
    let doc = Printf.sprintf("%s (Opium App)", name);
    let man = [];
    Term.info(name, ~doc, ~man);
  };
};

let run_command' = app => {
  open Cmdliner;
  let cmd = Cmds.term(app);
  switch (Term.eval((cmd, Cmds.info(app.name)))) {
  | `Ok(a) =>
    Lwt.async(() =>{
      let* _server = start(a);
      Lwt.return_unit;
   } );
    let (forever, _) = Lwt.wait();
    `Ok(forever);
  | `Error(_) => `Error
  | _ => `Not_running
  };
};

let run_command = app =>
  switch (app |> run_command') {
  | `Ok(a) =>
    Lwt.async(() =>{
      let* _server = a;
      Lwt.return_unit;
    });
    let (forever, _) = Lwt.wait();
    Lwt_main.run(forever);
  | `Error => exit(1)
  | `Not_running => exit(0)
  };

let run_multicore = app => {
  open Cmdliner;
  let cmd = Cmds.term(app);
  switch (Term.eval((cmd, Cmds.info(app.name)))) {
  | `Ok(a) => start_multicore(a)
  | `Error(_) => exit(1)
  | _ => exit(0)
  };
};
