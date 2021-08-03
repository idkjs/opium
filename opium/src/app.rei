/** An opium app provides a set of convenience functions and types to construct a rock
    app.

    - Re-exporting common functions needed in handlers
    - Easy handling of routes and bodies
    - Automatic generation of a command line app */;

/** An opium app is a simple builder wrapper around a rock app */

type t;

/** [to_handler t] converts the app t to a [Rock] handler. */

let to_handler: t => Rock.Handler.t;

/** A basic empty app */

let empty: t;

/** A builder is a function that transforms an [app] by adding some functionality.
    Builders are usuallys composed with a base app using (|>) to create a full app */

type builder = t => t;

let host: string => builder;

/** [backlog] specifies the maximum number of clients that can have a pending connection
    request to the Opium server. */

let backlog: int => builder;

let port: int => builder;
let jobs: int => builder;
let cmd_name: string => builder;

/** [not_found] accepts a regular Opium handler that will be used instead of the default
    404 handler. */

let not_found: (Request.t => Lwt.t((Headers.t, Body.t))) => builder;

/** A route is a function that returns a buidler that hooks up a handler to a url mapping */

type route = (string, Rock.Handler.t) => builder;

/** Method specific routes */;

let get: route;
let post: route;
let delete: route;
let put: route;
let options: route;
let head: route;
let patch: route;

/** any [methods] will bind a route to any http method inside of [methods] */

let any: list(Method.t) => route;

/** all [methods] will bind a route to a URL regardless of the http method. You may escape
    the actual method used from the request passed. */

let all: route;

let action: Method.t => route;
let middleware: Rock.Middleware.t => builder;

/** Start an opium server. The thread returned can be cancelled to shutdown the server */

let start: t => Lwt.t(Lwt_io.server);

/** Start an opium server with multiple processes. */

let start_multicore: t => unit;

/** Create a cmdliner command from an app and run lwt's event loop */

let run_command: t => unit;

/* Run a cmdliner command from an app. Does not launch Lwt's event loop. `Error is
   returned if the command line arguments are incorrect. `Not_running is returned if the
   command was completed without the server being launched */
let run_command': t => [> | `Ok(Lwt.t(unit)) | `Error | `Not_running];

/** Create a cmdliner command from an app and spawn with multiple processes. */

let run_multicore: t => unit;
