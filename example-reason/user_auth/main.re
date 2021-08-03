open Opium;

module User = {
  type t = {username: string};

  let t_of_sexp = sexp =>
    Sexplib0.Sexp.(
      switch (sexp) {
      | List([Atom("username"), Atom(username)]) => {username: username}
      | _ => failwith("invalid user sexp")
      }
    );

  let sexp_of_t = t =>
    Sexplib0.Sexp.(List([Atom("username"), Atom(t.username)]));
};

module Env_user = {
  type t = User.t;

  let key: Opium.Context.key(t) = (
    Opium.Context.Key.create(("user", User.sexp_of_t)): Opium.Context.key(t)
  );
};

let admin_handler = req => {
  let user = Opium.Context.find_exn(Env_user.key, req.Request.env);
  Response.of_plain_text(
    Printf.sprintf("Welcome back, %s!\n", user.username),
  )
  |> Lwt.return;
};

let unauthorized_handler = _req =>
  Response.of_plain_text(~status=`Unauthorized, "Unauthorized!\n")
  |> Lwt.return;

let auth_callback = (~username, ~password) =>
  switch (username, password) {
  | ("admin", "admin") => Lwt.return_some(User.{username: username})
  | _ => Lwt.return_none
  };

let auth_middleware =
  Middleware.basic_auth(
    ~key=Env_user.key,
    ~auth_callback,
    ~realm="my_realm",
    ~unauthorized_handler,
    (),
  );

let _ =
  App.empty
  |> App.middleware(auth_middleware)
  |> App.get("/", admin_handler)
  |> App.run_command;
