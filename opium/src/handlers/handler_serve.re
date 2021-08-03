open Import;

let respond_with_file = (~headers=?, ~read) => {
  open Lwt.Syntax;
  let* body = read();
  switch (body) {
  | Error(status) =>
    let headers = Option.value(headers, ~default=Httpaf.Headers.empty);
    let resp = Response.make(~headers, ~(status :> Httpaf.Status.t), ());
    Lwt.return(resp);
  | Ok(body) =>
    let headers = Option.value(headers, ~default=Httpaf.Headers.empty);
    let resp = Response.make(~headers, ~status=`OK, ~body, ());
    Lwt.return(resp);
  };
};

let h = (~mime_type=?, ~etag=?, ~headers=Httpaf.Headers.empty, read, req) => {
  let etag_quoted =
    switch (etag) {
    | Some(etag) => Some(Printf.sprintf("%S", etag))
    | None => None
    };

  let headers =
    switch (etag_quoted) {
    | Some(etag_quoted) =>
      Httpaf.Headers.add_unless_exists(headers, "ETag", etag_quoted)
    | None => headers
    };

  let headers =
    switch (mime_type) {
    | Some(mime_type) =>
      Httpaf.Headers.add_unless_exists(headers, "Content-Type", mime_type)
    | None => headers
    };

  let request_if_none_match =
    Httpaf.Headers.get(req.Request.headers, "If-None-Match");
  let request_matches_etag =
    switch (request_if_none_match, etag_quoted) {
    | (Some(request_etags), Some(etag_quoted)) =>
      request_etags
      |> String.split_on_char(~sep=',')
      |> List.exists(~f=request_etag =>
           String.trim(request_etag) == etag_quoted
         )
    | _ => false
    };

  if (request_matches_etag) {
    Lwt.return @@ Response.make(~status=`Not_modified, ~headers, ());
  } else {
    respond_with_file(~read, ~headers);
  };
};
