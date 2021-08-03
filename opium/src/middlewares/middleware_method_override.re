let m = {
  open Lwt.Syntax;
  let filter = (handler, req) => {
    let content_type = Request.content_type(req);
    switch (req.Request.meth, content_type) {
    | (`POST, Some("application/x-www-form-urlencoded")) =>
      let* method_result =
        Request.urlencoded("_method", req)
        |> Lwt.map(el => Option.map(String.uppercase_ascii, el));
      let method =
        switch (method_result) {
        | Some(m) =>
          switch (Method.of_string(m)) {
          | (`PUT | `DELETE | `Other("PATCH")) as m => m
          | _ => req.meth
          }
        | None => req.meth
        };

      handler({...req, meth: method});
    | _ => handler(req)
    };
  };

  Rock.Middleware.create(~name="Method override", ~filter);
};
