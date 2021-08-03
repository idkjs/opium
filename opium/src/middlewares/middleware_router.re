open Import;

module Method_map =
  Map.Make({
    type t = Method.t;

    let compare = (a, b) => {
      let left = String.uppercase_ascii(Method.to_string(a));
      let right = String.uppercase_ascii(Method.to_string(b));
      String.compare(left, right);
    };
  });

type t('a) = Method_map.t(list((Route.t, 'a)));

let empty = Method_map.empty;

let get = (t, meth) =>
  switch (Method_map.find_opt(meth, t)) {
  | None => []
  | Some(xs) => List.rev(xs)
  };

let add = (t, ~route, ~meth, ~action) =>
  Method_map.update(
    meth,
    fun
    | None => Some([(route, action)])
    | Some(xs) => Some([(route, action), ...xs]),
    t,
  );

/** finds matching endpoint and returns it with the parsed list of parameters */

let matching_endpoint = (endpoints, meth, uri) => {
  let endpoints = get(endpoints, meth);
  List.find_map(endpoints, ~f=ep =>
    uri |> Route.match_url(fst(ep)) |> Option.map(p => (ep, p))
  );
};

module Env = {
  let key: Context.key(Route.matches) = (
    Context.Key.create(("path_params", Route.sexp_of_matches)):
      Context.key(Route.matches)
  );
};

let splat = req =>
  Context.find_exn(Env.key, req.Request.env) |> (route => route.Route.splat);

/* not param_exn since if the endpoint was selected it's likely that the parameter is
   already there */
let param = (req, param) => {
  let {Route.params, _} = Context.find_exn(Env.key, req.Request.env);
  List.assoc(param, params);
};

let m = endpoints => {
  let filter = (default, req) =>
    switch (
      matching_endpoint(endpoints, req.Request.meth, req.Request.target)
    ) {
    | None => default(req)
    | Some((endpoint, params)) =>
      let env_with_params = Context.add(Env.key, params, req.Request.env);
      (snd(endpoint))({...req, Request.env: env_with_params});
    };

  Rock.Middleware.create(~name="Router", ~filter);
};
