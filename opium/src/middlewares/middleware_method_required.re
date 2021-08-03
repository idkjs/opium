open Import;

let allowed_methods = [`GET, `HEAD, `POST];

let m = (~allowed_methods=allowed_methods, ()) => {
  let filter = (handler, req) =>
    List.mem(req.Request.meth, ~set=allowed_methods)
      ? handler(req)
      : Lwt.return(Response.make(~status=`Method_not_allowed, ()));

  Rock.Middleware.create(~name="Method required", ~filter);
};
