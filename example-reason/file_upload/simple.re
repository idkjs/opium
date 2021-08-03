open Opium;

let layout = (~title as title_, children) =>
  Tyxml.Html.(
    html(
      ~a=[a_lang("en")],
      head(title(txt(title_)), []),
      body(children),
    )
  );

let index_view =
  Tyxml.Html.(
    layout(
      ~title="Opium file upload",
      [
        form(
          ~a=[
            a_action("/upload"),
            a_method(`Post),
            a_enctype("multipart/form-data"),
          ],
          [
            input(~a=[a_input_type(`File), a_name("file")], ()),
            button(~a=[a_button_type(`Submit)], [txt("Submit")]),
          ],
        ),
      ],
    )
  );

let index_handler = _request => Lwt.return @@ Response.of_html(index_view);

let upload_handler = request => {
  open Lwt.Syntax;
  let files = Hashtbl.create(~random=true, 5);
  let callback = (~name as _, ~filename, string) => {
    let filename = Filename.basename(filename);
    let write = file =>
      string
      |> String.length
      |> Lwt_unix.write_string(file, string, 0)
      |> Lwt.map(ignore);

    switch (Hashtbl.find_opt(files, filename)) {
    | Some(file) => write(file)
    | None =>
      let* file =
        Lwt_unix.openfile(
          filename,
          Unix.[O_CREAT, O_TRUNC, O_WRONLY, O_NONBLOCK],
          0o600,
        );
      Hashtbl.add(files, filename, file);
      write(file);
    };
  };

  let* _ = Request.to_multipart_form_data_exn(~callback, request);
  let close = (_, file, prev) => {
    let* () = prev;
    Lwt_unix.close(file);
  };

  let* () = Hashtbl.fold(close, files, Lwt.return_unit);
  Lwt.return @@ Response.of_plain_text("File uploaded successfully!");
};

let _ =
  App.empty
  |> App.get("/", index_handler)
  |> App.post("/upload", upload_handler)
  |> App.run_command;
