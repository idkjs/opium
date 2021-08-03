type t =
  pri {
    middlewares: list(Middleware.t),
    handler: Handler.t,
  };

let append_middleware: (t, Middleware.t) => t;
let create:
  (~middlewares: list(Middleware.t)=?, ~handler: Handler.t, unit) => t;
