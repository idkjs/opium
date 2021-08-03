open Opium;

let () = {
  let static =
    Middleware.static_unix(
      ~local_path="./example/static_serve/asset/",
      ~uri_prefix="/",
      (),
    );

  App.empty |> App.middleware(static) |> App.run_command;
};
