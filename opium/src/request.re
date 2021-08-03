open Import;
include Rock.Request;

let of_string' =
    (
      ~content_type="text/plain",
      ~version=?,
      ~env=?,
      ~headers=Headers.empty,
      target,
      meth,
      body,
    ) => {
  let headers =
    Headers.add_unless_exists(headers, "Content-Type", content_type);
  make(~version?, ~headers, ~body=Body.of_string(body), ~env?, target, meth);
};

let of_plain_text = (~version=?, ~headers=?, ~env=?, ~body, target, meth) =>
  of_string'(~version?, ~env?, ~headers?, target, meth, body);

let of_json = (~version=?, ~headers=?, ~env=?, ~body, target, meth) =>
  of_string'(
    ~content_type="application/json",
    ~version?,
    ~headers?,
    ~env?,
    target,
    meth,
    body |> Yojson.Safe.to_string,
  );

let of_urlencoded = (~version=?, ~headers=?, ~env=?, ~body, target, meth) =>
  of_string'(
    ~content_type="application/x-www-form-urlencoded",
    ~version?,
    ~headers?,
    ~env?,
    target,
    meth,
    body |> Uri.encoded_of_query,
  );

let to_json_exn = t => {
  open Lwt.Syntax;
  let* body = t.body |> Body.copy |> Body.to_string;
  Lwt.return @@ Yojson.Safe.from_string(body);
};

let to_json = t =>
  Lwt.Syntax.(
    Lwt.catch(
      () => {
        let+ json = to_json_exn(t);
        Some(json);
      },
      fun
      | _ => Lwt.return(None),
    )
  );

let to_plain_text = t => Body.copy(t.body) |> Body.to_string;

let to_urlencoded = t => {
  open Lwt.Syntax;
  let* body = t.body |> Body.copy |> Body.to_string;
  body |> Uri.query_of_encoded |> Lwt.return;
};

let header = (header, t) => Headers.get(t.headers, header);
let headers = (header, t) => Headers.get_multi(t.headers, header);
let add_header = ((k, v), t) => {
  ...t,
  headers: Headers.add(t.headers, k, v),
};

let add_header_or_replace = ((k, v), t) => {
  ...t,
  headers:
    if (Headers.mem(t.headers, k)) {
      Headers.replace(t.headers, k, v);
    } else {
      Headers.add(t.headers, k, v);
    },
};

let add_header_unless_exists = ((k, v), t) => {
  ...t,
  headers: Headers.add_unless_exists(t.headers, k, v),
};

let add_headers = (hs, t) => {
  ...t,
  headers: Headers.add_list(t.headers, hs),
};

let add_headers_or_replace = (hs, t) =>
  List.fold_left(hs, ~init=t, ~f=(acc, el) =>
    add_header_or_replace(el, acc)
  );

let add_headers_unless_exists = (hs, t) => {
  ...t,
  headers: Headers.add_list_unless_exists(t.headers, hs),
};

let remove_header = (key, t) => {
  ...t,
  headers: Headers.remove(t.headers, key),
};

let cookie = (~signed_with=?, cookie, t) =>
  Cookie.cookie_of_headers(
    ~signed_with?,
    cookie,
    t.headers |> Headers.to_list,
  )
  |> Option.map(snd);

let cookies = (~signed_with=?, t) =>
  Cookie.cookies_of_headers(~signed_with?, t.headers |> Headers.to_list);

let add_cookie = (~sign_with=?, (k, v), t) => {
  let cookies = cookies(t);
  let cookies =
    List.replace_or_add(
      ~f=((k2, _v2), _) => String.equal(k, k2),
      (
        k,
        switch (sign_with) {
        | Some(signer) => Cookie.Signer.sign(signer, v)
        | None => v
        },
      ),
      cookies,
    );

  let cookie_header =
    cookies |> List.map(~f=Cookie.make) |> Cookie.to_cookie_header;
  add_header_or_replace(cookie_header, t);
};

let add_cookie_unless_exists = (~sign_with=?, (k, v), t) => {
  let cookies = cookies(t);
  if (List.exists(cookies, ~f=((k2, _v2)) => String.equal(k2, k))) {
    t;
  } else {
    add_cookie(~sign_with?, (k, v), t);
  };
};

let remove_cookie = (key, t) => {
  let cookie_header =
    cookies(t)
    |> List.filter_map(~f=((k, v)) =>
         if (!String.equal(k, key)) {
           Some(Cookie.make((k, v)));
         } else {
           None;
         }
       )
    |> Cookie.to_cookie_header;

  add_header_or_replace(cookie_header, t);
};

let content_type = t => header("Content-Type", t);
let set_content_type = (s, t) => add_header(("Content-Type", s), t);

let authorization = t => {
  let s = header("Authorization", t);
  Option.map(Auth.credential_of_string, s);
};

let set_authorization = (cred, t) => {
  let s = Auth.string_of_credential(cred);
  add_header(("Authorization", s), t);
};

let to_multipart_form_data =
    (~callback=(~name as _, ~filename as _, _line) => Lwt.return_unit, t) =>
  switch (t.meth, content_type(t)) {
  | (`POST, Some(content_type))
      when
        String.is_prefix(
          content_type,
          ~prefix="multipart/form-data; boundary=",
        ) =>
    open Lwt.Syntax;
    let body = t.body |> Body.copy |> Body.to_stream;
    let* result =
      Multipart_form_data.parse(~stream=body, ~content_type, ~callback);
    Lwt.return @@ Some(result);
  | _ => Lwt.return(None)
  };

let to_multipart_form_data_exn = (~callback=?, t) => {
  open Lwt.Syntax;
  let* result = to_multipart_form_data(~callback?, t);
  switch (result) {
  | Some(r) => Lwt.return(r)
  | None =>
    raise(
      Invalid_argument(
        "The request is not a valid multipart/form-data request.",
      ),
    )
  };
};

let find_in_query = (key, query) =>
  query
  |> List.assoc_opt(key)
  |> (
    opt =>
      Option.bind(
        opt,
        fun
        | [] => None
        | [x, ..._] => Some(x),
      )
  );

let find_list_in_query = (key, query) =>
  query
  |> List.concat_map(~f=((k, v)) =>
       if (k == key) {
         v;
       } else {
         [];
       }
     );

let urlencoded = (key, t) => {
  open Lwt.Syntax;
  let* query = to_urlencoded(t);
  Lwt.return @@ find_in_query(key, query);
};

let urlencoded_list = (key, t) => {
  open Lwt.Syntax;
  let* query = to_urlencoded(t);
  Lwt.return @@ find_list_in_query(key, query);
};

let urlencoded_exn = (key, t) => {
  open Lwt.Syntax;
  let+ o = urlencoded(key, t);
  Option.get(o);
};

let query_list = t => t.target |> Uri.of_string |> Uri.query;
let query = (key, t) => query_list(t) |> find_in_query(key);
let query_exn = (key, t) => query(key, t) |> Option.get;

let sexp_of_t = ({version, target, headers, meth, body, env}) =>
  Sexp_conv.(
    Sexp.(
      List([
        List([Atom("version"), Version.sexp_of_t(version)]),
        List([Atom("target"), sexp_of_string(target)]),
        List([Atom("method"), Method.sexp_of_t(meth)]),
        List([Atom("headers"), Headers.sexp_of_t(headers)]),
        List([Atom("body"), Body.sexp_of_t(body)]),
        List([Atom("env"), Context.sexp_of_t(env)]),
      ])
    )
  );

let pp = (fmt, t) => Sexplib0.Sexp.pp_hum(fmt, sexp_of_t(t));

let pp_hum = (fmt, t) =>
  Format.fprintf(
    fmt,
    "%s %s %s\n%s\n\n%a\n%!",
    Method.to_string(t.meth),
    t.target,
    Version.to_string(t.version),
    Headers.to_string(t.headers),
    Body.pp_hum,
    t.body,
  );
