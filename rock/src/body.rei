/** Represents an HTTP request or response body. */;

type content = [
  | `Empty
  | `String(string)
  | `Bigstring(Bigstringaf.t)
  | /* TODO: switch to a iovec based stream */ `Stream(Lwt_stream.t(string))
];

/** [t] represents an HTTP message body. */

type t = {
  length: option(Int64.t),
  content,
};

/** {1 Constructor} */;

/** [of_string] creates a fixed length body from a string. */

let of_string: string => t;

/** [of_bigstring] creates a fixed length body from a bigstring. */

let of_bigstring: Bigstringaf.t => t;

/** [of_stream] takes a [string Lwt_stream.t] and creates a HTTP body from it. */

let of_stream: (~length: Int64.t=?, Lwt_stream.t(string)) => t;

/** [empty] represents a body of size 0L. */

let empty: t;

/** [copy t] creates a new instance of the body [t]. If the body is a stream, it is be
    duplicated safely and the initial stream will remain untouched. */

let copy: t => t;

/** {1 Decoders} */;

/** [to_string t] returns a promise that will eventually be filled with a string
    representation of the body. */

let to_string: t => Lwt.t(string);

/** [to_stream t] converts the body to a [string Lwt_stream.t]. */

let to_stream: t => Lwt_stream.t(string);

/** {1 Getters and Setters} */;

let length: t => option(Int64.t);

/** {1 Utilities} */;

/** [drain t] will repeatedly read values from the body stream and discard them. */

let drain: t => Lwt.t(unit);

/** [sexp_of_t t] converts the body [t] to an s-expression */

let sexp_of_t: t => Sexplib0.Sexp.t;

/** [pp] formats the body [t] as an s-expression */

[@ocaml.toplevel_printer]
let pp: (Format.formatter, t) => unit;

/** [pp_hum] formats the body [t] as an string.

    If the body content is a stream, the pretty printer will output the value ["<stream>"]*/

[@ocaml.toplevel_printer]
let pp_hum: (Format.formatter, t) => unit;
