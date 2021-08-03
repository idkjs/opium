type content = [
  | `Empty
  | `String(string)
  | `Bigstring(Bigstringaf.t)
  | /* TODO: switch to a iovec based stream */ `Stream(Lwt_stream.t(string))
];

type t = {
  length: option(Int64.t),
  content,
};

let length = t => t.length;

let escape_html = s => {
  let b = Buffer.create(42);
  for (i in 0 to String.length(s) - 1) {
    switch (s.[i]) {
    | ('&' | '<' | '>' | '\'' | '"') as c =>
      Printf.bprintf(b, "&#%d;", int_of_char(c))
    | c => Buffer.add_char(b, c)
    };
  };
  Buffer.contents(b);
};

let sexp_of_content = content =>
  Sexplib0.Sexp_conv.(
    switch (content) {
    | `Empty => sexp_of_string("")
    | `String(s) => sexp_of_string(escape_html(s))
    | `Bigstring(b) => sexp_of_string(escape_html(Bigstringaf.to_string(b)))
    | `Stream(s) => sexp_of_opaque(s)
    }
  );

let sexp_of_t = ({length, content}) => {
  open Sexplib0;
  let len = Sexp_conv.sexp_of_option(Sexp_conv.sexp_of_int64);
  Sexp.(
    List([
      List([Atom("length"), len(length)]),
      List([Atom("content"), sexp_of_content(content)]),
    ])
  );
};

let drain = ({content, _}) =>
  switch (content) {
  | `Stream(stream) => Lwt_stream.junk_while(_ => true, stream)
  | _ => Lwt.return_unit
  };

let to_string = ({content, _}) =>
  Lwt.Syntax.(
    switch (content) {
    | `Stream(content) =>
      let buf = Buffer.create(1024);
      let+ () = Lwt_stream.iter(s => Buffer.add_string(buf, s), content);
      Buffer.contents(buf);
    | `String(s) => Lwt.return(s)
    | `Bigstring(b) => Lwt.return(Bigstringaf.to_string(b))
    | `Empty => Lwt.return("")
    }
  );

let to_stream = ({content, _}) =>
  switch (content) {
  | `Empty => Lwt_stream.of_list([])
  | `String(s) => Lwt_stream.of_list([s])
  | `Bigstring(b) => Lwt_stream.of_list([Bigstringaf.to_string(b)])
  | `Stream(s) => s
  };

let len = x => Some(Int64.of_int(x));
let of_string = s => {content: `String(s), length: len(String.length(s))};
let of_bigstring = b => {
  content: `Bigstring(b),
  length: len(Bigstringaf.length(b)),
};
let empty = {content: `Empty, length: Some(0L)};
let of_stream = (~length=?, s) => {content: `Stream(s), length};

let copy = t =>
  switch (t.content) {
  | `Empty => t
  | `String(_) => t
  | `Bigstring(_) => t
  | `Stream(stream) => {...t, content: `Stream(Lwt_stream.clone(stream))}
  };

let pp = (fmt, t) => Sexplib0.Sexp.pp_hum(fmt, sexp_of_t(t));

let pp_hum = (fmt, t) =>
  Format.fprintf(
    fmt,
    "%s",
    switch (t.content) {
    | `Empty => ""
    | `String(s) => s
    | `Bigstring(b) => Bigstringaf.to_string(b)
    | `Stream(_) => "<stream>"
    },
  );
