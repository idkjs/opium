/** Collection of functions to run a server from a Rock app. */;

type error_handler =
  (Httpaf.Headers.t, Httpaf.Server_connection.error) =>
  Lwt.t((Httpaf.Headers.t, Body.t));

let run:
  (
    (
      ~request_handler: Httpaf.Server_connection.request_handler,
      ~error_handler: Httpaf.Server_connection.error_handler
    ) =>
    Lwt.t('a),
    ~error_handler: error_handler=?,
    App.t
  ) =>
  Lwt.t('a);

/** The Halt exception can be raised to interrupt the normal processing flow of a request.

    The exception will be handled by the main run function (in {!Server_connection.run})
    and the response will be sent to the client directly.

    This is especially useful when you want to make sure that no other middleware will be
    able to modify the response. */

exception Halt(Response.t);

/** Raises a Halt exception to interrupt the processing of the connection and trigger an
    early response. */

let halt: Response.t => 'a;
