let m = (~unauthorized_handler=?, ~key, ~realm, ~auth_callback, ()) => {
  let unauthorized_handler =
    Option.value(unauthorized_handler, ~default=_req =>
      Response.of_plain_text("Forbidden access", ~status=`Unauthorized)
      |> Response.add_header((
           "WWW-Authenticate",
           Auth.string_of_challenge(Basic(realm)),
         ))
      |> Lwt.return
    );

  let filter = (handler, {Request.env, _} as req) => {
    open Lwt.Syntax;
    let+ resp =
      switch (Request.authorization(req)) {
      | None => unauthorized_handler(req)
      | Some(Other(_)) => unauthorized_handler(req)
      | Some([@implicit_arity] Basic(username, password)) =>
        let* user_opt = auth_callback(~username, ~password);
        switch (user_opt) {
        | None => unauthorized_handler(req)
        | Some(user) =>
          let env = Context.add(key, user, env);
          let req = {...req, Request.env};
          handler(req);
        };
      };
    switch (resp.Response.status) {
    | `Unauthorized =>
      Response.add_header(
        ("WWW-Authenticate", Auth.string_of_challenge(Basic(realm))),
        resp,
      )
    | _ => resp
    };
  };

  Rock.Middleware.create(~name="Basic authentication", ~filter);
};
