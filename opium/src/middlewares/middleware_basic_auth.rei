let m:
  (
    ~unauthorized_handler: Rock.Handler.t=?,
    ~key: Context.key('a),
    ~realm: string,
    ~auth_callback: (~username: string, ~password: string) =>
                    Lwt.t(option('a)),
    unit
  ) =>
  Rock.Middleware.t;
