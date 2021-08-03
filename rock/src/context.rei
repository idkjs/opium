/** A context holds heterogeneous value and is passed to the requests or responses. */;

/** {2:keys Keys} */;

/** The type for keys whose lookup value is of type ['a]. */

type key('a);

/** {3 [Key]} */;

module Key: {
  /** {2:keys Keys} */;

  /** The type for key information. */

  type info('a) = (string, 'a => Sexplib0.Sexp.t);

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

  type t;

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

/** {1:maps Maps} */;

/** The type for heterogeneous value maps. */

type t;

/** [empty] is the empty map. */

let empty: t;

/** [is_empty m] is [true] iff [m] is empty. */

let is_empty: t => bool;

/** [mem k m] is [true] iff [k] is bound in [m]. */

let mem: (key('a), t) => bool;

/** [add k v m] is [m] with [k] bound to [v]. */

let add: (key('a), 'a, t) => t;

/** [singleton k v] is [add k v empty]. */

let singleton: (key('a), 'a) => t;

/** [rem k m] is [m] with [k] unbound. */

let rem: (key('a), t) => t;

/** [find k m] is the value of [k]'s binding in [m], if any. */

let find: (key('a), t) => option('a);

/** [get k m] is the value of [k]'s binding in [m].

    @raise Invalid_argument if [k] is not bound in [m]. */

let get: (key('a), t) => 'a;

/** The type for bindings. */

type binding =
  | B(key('a), 'a): binding;

/** [iter f m] applies [f] to all bindings of [m]. */

let iter: (binding => unit, t) => unit;

/** [fold f m acc] folds over the bindings of [m] with [f], starting with [acc] */

let fold: ((binding, 'a) => 'a, t, 'a) => 'a;

/** [for_all p m] is [true] iff all bindings of [m] satisfy [p]. */

let for_all: (binding => bool, t) => bool;

/** [exists p m] is [true] iff there exists a bindings of [m] that satisfies [p]. */

let exists: (binding => bool, t) => bool;

/** [filter p m] are the bindings of [m] that satisfy [p]. */

let filter: (binding => bool, t) => t;

/** [cardinal m] is the number of bindings in [m]. */

let cardinal: t => int;

/** [any_binding m] is a binding of [m] (if not empty). */

let any_binding: t => option(binding);

/** [get_any_binding m] is a binding of [m].

    @raise Invalid_argument if [m] is empty. */

let get_any_binding: t => binding;
