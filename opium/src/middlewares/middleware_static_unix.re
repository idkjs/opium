let m = (~local_path, ~uri_prefix=?, ~headers=?, ~etag_of_fname=?, ()) => {
  open Lwt.Syntax;
  let read = fname => {
    let* body = Body.of_file(Filename.concat(local_path, fname));
    switch (body) {
    | None => Lwt.return(Error(`Not_found))
    | Some(body) => Lwt.return(Ok(body))
    };
  };

  Middleware_static.m(~read, ~uri_prefix?, ~headers?, ~etag_of_fname?, ());
};
