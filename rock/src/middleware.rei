/** Middleware is a named, simple filter, that only works on rock requests/response. */;

type t =
  pri {
    filter: Filter.simple(Request.t, Response.t),
    name: string,
  };

let create:
  (~filter: Filter.simple(Request.t, Response.t), ~name: string) => t;
let apply: (t, Handler.t) => Handler.t;
