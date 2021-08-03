let h:
  (
    ~mime_type: string=?,
    ~etag: string=?,
    ~headers: Headers.t=?,
    unit => Lwt_result.t(Body.t, [ Status.client_error | Status.server_error])
  ) =>
  Rock.Handler.t;
