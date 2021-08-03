type t = {
  middlewares: list(Middleware.t),
  handler: Handler.t,
};

let append_middleware = (t, m) => {...t, middlewares: t.middlewares @ [m]};
let create = (~middlewares=[], ~handler, ()) => {middlewares, handler};
