/** Authentication functions to work with common HTTP authentication methods. */;

module Challenge: {
  type t =
    | Basic(string);

  /** {3 [t_of_sexp]} */;

  /** [t_of_sexp sexp] parses the s-expression [sexp] into a challenge */

  let t_of_sexp: Sexplib0.Sexp.t => t;

  /** {3 [sexp_of_t]} */;

  /** [sexp_of_t t] converts the challenge [t] to an s-expression */

  let sexp_of_t: t => Sexplib0.Sexp.t;
};

module Credential: {
  type t =
    | Basic(string, string) /* username, password */
    | Other(string);

  /** {3 [t_of_sexp]} */;

  /** [t_of_sexp sexp] parses the s-expression [sexp] into credentials */

  let t_of_sexp: Sexplib0.Sexp.t => t;

  /** {3 [sexp_of_t]} */;

  /** [sexp_of_t t] converts the credentials [t] to an s-expression */

  let sexp_of_t: t => Sexplib0.Sexp.t;
};

/** {3 [string_of_credential]} */;

/** [string_of_credential cred] converts the credentials into a string usable in the
    [Authorization] header. */

let string_of_credential: Credential.t => string;

/** {3 [credential_of_string]} */;

/** [credential_of_string s] parses a string from the [Authorization] header into
    credentials. */

let credential_of_string: string => Credential.t;

/** {3 [string_of_challenge]} */;

/** [string_of_challenge challenge] converts the challenge into a string usable in the
    [WWW-Authenticate] response header. */

let string_of_challenge: Challenge.t => string;
