module Challenge = {
  type t =
    | Basic(string);

  let t_of_sexp =
    Sexplib0.Sexp.(
      fun
      | List([Atom("basic"), Atom(s)]) => Basic(s)
      | _ => failwith("invalid challenge sexp")
    );

  let sexp_of_t =
    Sexplib0.Sexp.(
      fun
      | Basic(s) => List([Atom("basic"), Atom(s)])
    );
};

module Credential = {
  type t =
    | Basic(string, string) /* username, password */
    | Other(string);

  let t_of_sexp =
    Sexplib0.Sexp.(
      fun
      | List([Atom("basic"), Atom(u), Atom(p)]) =>
        [@implicit_arity] Basic(u, p)
      | _ => failwith("invalid credential sexp")
    );

  let sexp_of_t =
    Sexplib0.Sexp.(
      fun
      | [@implicit_arity] Basic(u, p) =>
        List([Atom("basic"), Atom(u), Atom(p)])
      | Other(s) => List([Atom("other"), Atom(s)])
    );
};

let string_of_credential = (cred: Credential.t) =>
  switch (cred) {
  | [@implicit_arity] Basic(user, pass) =>
    "Basic " ++ Base64.encode_string(Printf.sprintf("%s:%s", user, pass))
  | Other(buf) => buf
  };

let credential_of_string = (buf: string): Credential.t =>
  try({
    let b64 = Scanf.sscanf(buf, "Basic %s", b => b);
    switch (Stringext.split(~on=':', Base64.decode_exn(b64), ~max=2)) {
    | [user, pass] => [@implicit_arity] Basic(user, pass)
    | _ => Other(buf)
    };
  }) {
  | _ => Other(buf)
  };

let string_of_challenge =
  fun
  | Challenge.Basic(realm) => Printf.sprintf("Basic realm=\"%s\"", realm);
