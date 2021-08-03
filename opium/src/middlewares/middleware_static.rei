let m:
  (
    ~read: string =>
           Lwt_result.t(Body.t, [ Status.client_error | Status.server_error]),
    ~uri_prefix: string=?,
    ~headers: Headers.t=?,
    ~etag_of_fname: string => option(string)=?,
    unit
  ) =>
  Rock.Middleware.t;
