/* A major part of this documentation is extracted from
   {{:https://github.com/inhabitedtype/httpaf/blob/master/lib/httpaf.mli}.

   Copyright (c) 2016, Inhabited Type LLC

   All rights reserved.*/

/** Protocol Version

    HTTP uses a "<major>.<minor>" numbering scheme to indicate versions of the protocol.
    The protocol version as a whole indicates the sender's conformance with the set of
    requirements laid out in that version's corresponding specification of HTTP.

    See {{:https://tools.ietf.org/html/rfc7230#section-2.6} RFC7230§2.6} for more
    details. */;

type t =
  Httpaf.Version.t = {
    major: int,
    minor: int,
  };

/** [compare x y] returns [0] if version [x] is equal to version [y], a negative integer
    if version [x] is less than version [y], and a positive integer if version [x] is
    greater than version [y]. */

let compare: (t, t) => int;

/** [to_string t] returns a string representation of the version [t]. */

let to_string: t => string;

/** [of_string s] returns a version from its string representation [s]. */

let of_string: string => t;

/** {2 Utilities} */;

/** [sexp_of_t t] converts the request [t] to an s-expression */

let sexp_of_t: t => Sexplib0.Sexp.t;

/** [pp] formats the request [t] as an s-expression */

[@ocaml.toplevel_printer]
let pp: (Format.formatter, t) => unit;

/** [pp_hum] formats the request [t] as a standard HTTP request */

[@ocaml.toplevel_printer]
let pp_hum: (Format.formatter, t) => unit;
