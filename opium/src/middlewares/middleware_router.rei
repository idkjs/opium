type t('a);

let m: t(Rock.Handler.t) => Rock.Middleware.t;
let empty: t('action);
let add: (t('a), ~route: Route.t, ~meth: Method.t, ~action: 'a) => t('a);
let param: (Request.t, string) => string;
let splat: Request.t => list(string);
