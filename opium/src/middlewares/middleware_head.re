/* The implementation of this middleware is based on Finagle's HeadFilter
   https://github.com/twitter/finagle/blob/develop/finagle-http/src/main/scala/com/twitter/finagle/http/filter/HeadFilter.scala

   It has to be before {!Middleware_content_length} */

let m = {
  open Lwt.Syntax;
  let filter = (handler, req) => {
    let req =
      switch (req.Request.meth) {
      | `HEAD => {...req, meth: `GET}
      | _ => req
      };

    let* response = handler(req);
    let body_length = Body.length(response.Response.body);
    let response =
      switch (body_length) {
      | Some(l) =>
        {...response, body: Body.empty}
        |> Response.add_header_or_replace((
             "Content-Length",
             Int64.to_string(l),
           ))
        |> Response.remove_header("Content-Encoding")
      | None =>
        {...response, body: Body.empty}
        |> Response.remove_header("Content-Length")
        |> Response.remove_header("Content-Encoding")
      };

    Lwt.return(response);
  };

  Rock.Middleware.create(~name="Head", ~filter);
};
