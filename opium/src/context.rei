/** A context holds heterogeneous value and is passed to the requests or responses. */;

/** {2:keys Keys} */;

/** The type for keys whose lookup value is of type ['a]. */

type key('a) = Rock.Context.key('a);

/** {3 [Key]} */;

module Key: {
  /** {2:keys Keys} */;

  /** The type for key information. */

  type info('a) = Rock.Context.Key.info('a);

  /** {3 [create]} */;

  /** [create i] is a new key with information [i]. */

  let create: info('a) => key('a);

  /** {3 [info]} */;

  /** [info k] is [k]'s information. */

  let info: key('a) => info('a);

  /** {2:exists Existential keys}

      Exisential keys allow to compare keys. This can be useful for functions like
      {!filter}. */;

  /** The type for existential keys. */

  type t = Rock.Context.Key.t;

  /** {3 [hide_type]} */;

  /** [hide_type k] is an existential key for [k]. */

  let hide_type: key('a) => t;

  /** {3 [equal]} */;

  /** [equal k k'] is [true] iff [k] and [k'] are the same key. */

  let equal: (t, t) => bool;

  /** {3 [compare]} */;

  /** [compare k k'] is a total order on keys compatible with {!equal}. */

  let compare: (t, t) => int;
};

/** {2:maps Maps} */;

/** The type for heterogeneous value maps. */

type t = Rock.Context.t;

/** {3 [empty]} */;

/** [empty] is the empty map. */

let empty: t;

/** {3 [is_empty]} */;

/** [is_empty m] is [true] iff [m] is empty. */

let is_empty: t => bool;

/** {3 [mem]} */;

/** [mem k m] is [true] iff [k] is bound in [m]. */

let mem: (key('a), t) => bool;

/** {3 [add]} */;

/** [add k v m] is [m] with [k] bound to [v]. */

let add: (key('a), 'a, t) => t;

/** {3 [singleton]} */;

/** [singleton k v] is [add k v empty]. */

let singleton: (key('a), 'a) => t;

/** {3 [rem]} */;

/** [rem k m] is [m] with [k] unbound. */

let rem: (key('a), t) => t;

/** {3 [find]} */;

/** [find k m] is the value of [k]'s binding in [m], if any. */

let find: (key('a), t) => option('a);

/** {3 [find_exn]} */;

/** [find_exn k m] is the value of [k]'s binding find_exn [m].

    @raise Invalid_argument if [k] is not bound in [m]. */

let find_exn: (key('a), t) => 'a;

/** {2:utilities Utilities} */;

/** {3 [sexp_of_t]} */;

/** [sexp_of_t t] converts the request [t] to an s-expression */

let sexp_of_t: t => Sexplib0.Sexp.t;

/** {3 [pp_hum]} */;

/** [pp_hum] formats the request [t] as a standard HTTP request */

[@ocaml.toplevel_printer]
let pp_hum: (Format.formatter, t) => unit;
