open Opium;

module Schema = {
  open Graphql_lwt;

  type context = unit;

  type role =
    | User
    | Admin;

  type user = {
    id: int,
    name: string,
    role,
  };

  let users = [
    {id: 1, name: "Alice", role: Admin},
    {id: 2, name: "Bob", role: User},
  ];

  let role: Graphql_lwt.Schema.typ(context, option(role)) = (
    Schema.(
      enum(
        "role",
        ~doc="The role of a user",
        ~values=[
          enum_value("USER", ~value=User),
          enum_value("ADMIN", ~value=Admin),
        ],
      )
    ):
      Graphql_lwt.Schema.typ(context, option(role))
  );

  let user: Graphql_lwt.Schema.typ(context, option(user)) = (
    Schema.(
      obj("user", ~doc="A user in the system", ~fields=_ =>
        [
          field(
            "id",
            ~doc="Unique user identifier",
            ~typ=non_null(int),
            ~args=Arg.[],
            ~resolve=(_info, p) =>
            p.id
          ),
          field(
            "name", ~typ=non_null(string), ~args=Arg.[], ~resolve=(_info, p) =>
            p.name
          ),
          field(
            "role", ~typ=non_null(role), ~args=Arg.[], ~resolve=(_info, p) =>
            p.role
          ),
        ]
      )
    ):
      Graphql_lwt.Schema.typ(context, option(user))
  );

  let schema =
    Graphql_lwt.Schema.(
      schema([
        field(
          "users",
          ~typ=non_null(list(non_null(user))),
          ~args=Arg.[],
          ~resolve=(_info, ()) =>
          users
        ),
      ])
    );
};

let graphql = {
  let handler =
    Opium_graphql.make_handler(~make_context=_req => (), Schema.schema);
  Opium.App.all("/", handler);
};

let graphiql = {
  let handler = Opium_graphql.make_graphiql_handler(~graphql_endpoint="/");
  Opium.App.get("/graphiql", handler);
};

let _ = {
  Logs.set_reporter(Logs_fmt.reporter());
  Logs.set_level(Some(Logs.Debug));
  App.empty |> graphql |> graphiql |> App.run_command;
};
