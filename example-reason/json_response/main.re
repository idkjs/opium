open Opium;

let sys_json = _req => {
  let json: Yojson.Safe.t = (
    `Assoc([
      ("os-type", `String(Sys.os_type)),
      ("ocaml-version", `String(Sys.ocaml_version)),
    ]): Yojson.Safe.t
  );

  Response.of_json(json) |> Lwt.return;
};

let _ = App.empty |> App.get("/", sys_json) |> App.run_command;
