open Import;

let log_src =
  Logs.Src.create(
    ~doc="Opium middleware to server static files",
    "opium.static_server",
  );

module Log = (val Logs.src_log(log_src): Logs.LOG);

let is_prefix = (~prefix, s) =>
  String.length(prefix) <= String.length(s)
  && {
    let i = ref(0);
    while (i^ < String.length(prefix) && s.[i^] == prefix.[i^]) {
      incr(i);
    };
    i^ == String.length(prefix);
  };

let chop_prefix = (~prefix, s) => {
  assert(is_prefix(~prefix, s));
  String.sub(
    s,
    ~pos=String.length(prefix),
    ~len=String.(length(s) - length(prefix)),
  );
};

let _add_opt_header_unless_exists = (headers, k, v) =>
  switch (headers) {
  | Some(h) => Httpaf.Headers.add_unless_exists(h, k, v)
  | None => Httpaf.Headers.of_list([(k, v)])
  };

let m = (~read, ~uri_prefix="/", ~headers=?, ~etag_of_fname=?, ()) => {
  open Lwt.Syntax;
  let filter = (handler, req) =>
    if (req.Request.meth == `GET) {
      let local_path = req.target;
      if (local_path |> is_prefix(~prefix=uri_prefix)) {
        let legal_path = chop_prefix(local_path, ~prefix=uri_prefix);
        let read = () => read(legal_path);
        let mime_type = Magic_mime.lookup(legal_path);
        let etag =
          switch (etag_of_fname) {
          | Some(f) => f(legal_path)
          | None => None
          };

        let* res = Handler_serve.h(read, ~mime_type, ~etag?, ~headers?, req);
        switch (res.status) {
        | `Not_found => handler(req)
        | _ => Lwt.return(res)
        };
      } else {
        handler(req);
      };
    } else {
      handler(req);
    };

  Rock.Middleware.create(~name="Static", ~filter);
};
