open Import;
include Rock.Context;

let sexp_of_t = m => {
  open Sexp;
  let l =
    fold(
      ([@implicit_arity] B(k, v), l) => {
        let (name, to_sexp) = Key.info(k);
        [List([Atom(name), to_sexp(v)]), ...l];
      },
      m,
      [],
    );

  List(l);
};

let pp_hum = (fmt, t) => Sexp.pp_hum(fmt, sexp_of_t(t));

let find_exn = (t, k) =>
  switch (find(t, k)) {
  | None => raise(Not_found)
  | Some(s) => s
  };
