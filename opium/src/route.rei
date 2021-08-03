/** Expression that represent a target or multiple */;

type t;

type matches = {
  params: list((string, string)),
  splat: list(string),
};

/** [sexp_of_t matches] converts the matches [matches] to an s-expression */

let sexp_of_matches: matches => Sexplib0.Sexp.t;

/** [of_string s] returns a route from its string representation [s]. */

let of_string: string => t;

/** [to_string t] returns a string representation of the route [t]. */

let to_string: t => string;

/** [match_url t url] return the matches of the url [url] for the route [t], or [None] if
    the url does not match. */

let match_url: (t, string) => option(matches);
