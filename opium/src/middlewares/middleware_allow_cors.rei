let m:
  (
    ~origins: list(String.t)=?,
    ~credentials: bool=?,
    ~max_age: int=?,
    ~headers: list(string)=?,
    ~expose: list(string)=?,
    ~methods: list(Method.t)=?,
    ~send_preflight_response: bool=?,
    unit
  ) =>
  Rock.Middleware.t;
