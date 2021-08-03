let m:
  (
    ~local_path: string,
    ~uri_prefix: string=?,
    ~headers: Headers.t=?,
    ~etag_of_fname: string => option(string)=?,
    unit
  ) =>
  Rock.Middleware.t;
