include Rock.Body;

let log_src = Logs.Src.create("opium.body.of_file");

module Log = (val Logs.src_log(log_src): Logs.LOG);

exception Isnt_a_file;

let of_file = fname => {
  open Lwt.Syntax;
  /* TODO: allow buffer size to be configurable */
  let bufsize = 4096;
  Lwt.catch(
    () => {
      let* s = Lwt_unix.stat(fname);
      let* () =
        if (Unix.(s.st_kind != S_REG)) {
          Lwt.fail(Isnt_a_file);
        } else {
          Lwt.return_unit;
        };
      let* ic =
        Lwt_io.open_file(
          ~buffer=Lwt_bytes.create(bufsize),
          ~flags=[O_RDONLY],
          ~mode=Lwt_io.input,
          fname,
        );
      let+ size = Lwt_io.length(ic);
      let stream =
        Lwt_stream.from(() =>
          Lwt.catch(
            () => {
              let+ b = Lwt_io.read(~count=bufsize, ic);
              switch (b) {
              | "" => None
              | buf => Some(buf)
              };
            },
            exn => {
              Log.warn(m =>
                m(
                  "Error while reading file %s. %s",
                  fname,
                  Printexc.to_string(exn),
                )
              );
              Lwt.return_none;
            },
          )
        );

      Lwt.on_success(Lwt_stream.closed(stream), () =>
        Lwt.async(() => Lwt_io.close(ic))
      );
      Some(of_stream(~length=size, stream));
    },
    e =>
      switch (e) {
      | Isnt_a_file
      | [@implicit_arity] Unix.Unix_error(Unix.ENOENT, _, _) =>
        Lwt.return(None)
      | exn =>
        Logs.err(m =>
          m(
            "Unknown error while serving file %s. %s",
            fname,
            Printexc.to_string(exn),
          )
        );
        Lwt.fail(exn);
      },
  );
};
