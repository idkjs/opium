open Import;

/* TODO: The non-cached responses should include Cache-Control, Content-Location, Date,
   ETag, Expires, and Vary */

let etag_of_body = body => {
  let encode = s =>
    s
    |> Cstruct.of_string
    |> Mirage_crypto.Hash.digest(`MD5)
    |> Cstruct.to_string
    |> Base64.encode_exn;

  switch (body.Body.content) {
  | `String(s) => Some(encode(s))
  | `Bigstring(b) => Some(b |> Bigstringaf.to_string |> encode)
  | `Empty => Some(encode(""))
  | `Stream(_) => None
  };
};

let m = {
  let filter = (handler, req) => {
    open Lwt.Syntax;
    let* response = handler(req);
    switch (response.Response.status) {
    | `OK
    | `Created
    | `Accepted =>
      let etag_quoted =
        switch (etag_of_body(response.Response.body)) {
        | Some(etag) => Some(Printf.sprintf("%S", etag))
        | None => None
        };

      let response =
        switch (etag_quoted) {
        | Some(etag_quoted) =>
          Response.add_header_or_replace(("ETag", etag_quoted), response)
        | None => response
        };

      let request_if_none_match = Response.header("If-None-Match", response);
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
        Lwt.return @@
        Response.make(~status=`Not_modified, ~headers=response.headers, ());
      } else {
        Lwt.return(response);
      };
    | _ => Lwt.return(response)
    };
  };

  Rock.Middleware.create(~name="ETag", ~filter);
};
