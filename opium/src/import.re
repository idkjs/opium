include Sexplib0;

module List = {
  include ListLabels;

  let rec filter_opt =
    fun
    | [] => []
    | [None, ...l] => filter_opt(l)
    | [Some(x), ...l] => [x, ...filter_opt(l)];

  let rec find_map = (~f) =>
    fun
    | [] => None
    | [x, ...l] =>
      switch (f(x)) {
      | Some(_) as result => result
      | None => find_map(~f, l)
      };

  let replace_or_add = (~f, to_add, l) => {
    let rec aux = (acc, l, found) =>
      switch (l) {
      | [] =>
        rev(
          if (!found) {
            [to_add, ...acc];
          } else {
            acc;
          },
        )
      | [el, ...rest] =>
        if (f(el, to_add)) {
          aux([to_add, ...acc], rest, true);
        } else {
          aux([el, ...acc], rest, found);
        }
      };

    aux([], l, false);
  };

  let concat_map = (~f, l) => {
    let rec aux = (f, acc) =>
      fun
      | [] => rev(acc)
      | [x, ...l] => {
          let xs = f(x);
          aux(f, rev_append(xs, acc), l);
        };

    aux(f, [], l);
  };
};

module String = {
  include StringLabels;

  let rec check_prefix = (s, ~prefix, len, i) =>
    i == len || s.[i] == prefix.[i] && check_prefix(s, ~prefix, len, i + 1);

  let is_prefix = (s, ~prefix) => {
    let len = length(s);
    let prefix_len = length(prefix);
    len >= prefix_len && check_prefix(s, ~prefix, prefix_len, 0);
  };
};
