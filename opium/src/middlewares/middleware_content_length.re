let m = {
  open Lwt.Syntax;
  let filter = (handler, req) => {
    let+ res = handler(req);
    let length = Body.length(res.Response.body);
    switch (length) {
    | None =>
      res
      |> Response.remove_header("Content-Length")
      |> Response.add_header_unless_exists(("Transfer-Encoding", "chunked"))
    | Some(l) =>
      Response.add_header_or_replace(
        ("Content-Length", Int64.to_string(l)),
        res,
      )
    };
  };

  Rock.Middleware.create(~name="Content length", ~filter);
};
