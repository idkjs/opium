open Import;

type path_segment =
  | Match(string)
  | Param(string)
  | Splat
  | FullSplat
  | Slash;

type matches = {
  params: list((string, string)),
  splat: list(string),
};

let sexp_of_matches = ({params, splat}) => {
  let splat' = Sexp_conv.sexp_of_list(Sexp_conv.sexp_of_string, splat);
  let sexp_of_param = ((a, b)) =>
    Sexp_conv.sexp_of_list(Sexp_conv.sexp_of_string, [a, b]);
  Sexp.List([
    List([Atom("params"), Sexp_conv.sexp_of_list(sexp_of_param, params)]),
    List([Atom("splat"), splat']),
  ]);
};

type t = list(path_segment);

let parse_param = s =>
  if (s == "/") {
    Slash;
  } else if (s == "*") {
    Splat;
  } else if (s == "**") {
    FullSplat;
  } else {
    try(Scanf.sscanf(s, ":%s", s => Param(s))) {
    | Scanf.Scan_failure(_) => Match(s)
    };
  };

let of_list = l => {
  let last_i = List.length(l) - 1;
  l
  |> List.mapi(~f=(i, s) =>
       switch (parse_param(s)) {
       | FullSplat when i != last_i =>
         invalid_arg("** is only allowed at the end")
       | x => x
       }
     );
};

let split_slash_delim = {
  let re = '/' |> Re.char |> Re.compile;
  path =>
    path
    |> Re.split_full(re)
    |> List.map(
         ~f=
           fun
           | `Text(s) => `Text(s)
           | `Delim(_) => `Delim,
       );
};

let split_slash = path =>
  path
  |> split_slash_delim
  |> List.map(
       ~f=
         fun
         | `Text(s) => s
         | `Delim => "/",
     );

let of_string = path => path |> split_slash |> of_list;

let to_string = l => {
  let r =
    l
    |> List.filter_map(
         ~f=
           fun
           | Match(s) => Some(s)
           | Param(s) => Some(":" ++ s)
           | Splat => Some("*")
           | FullSplat => Some("**")
           | Slash => None,
       )
    |> String.concat(~sep="/");

  "/" ++ r;
};

let rec match_url = (t, url, {params, splat} as matches) =>
  switch (t, url) {
  | ([], [])
  | ([FullSplat], _) => Some(matches)
  | ([FullSplat, ..._], _) => assert(false) /* splat can't be last */
  | ([Match(x), ...t], [`Text(y), ...url]) when x == y =>
    match_url(t, url, matches)
  | ([Slash, ...t], [`Delim, ...url]) => match_url(t, url, matches)
  | ([Splat, ...t], [`Text(s), ...url]) =>
    match_url(t, url, {...matches, splat: [Uri.pct_decode(s), ...splat]})
  | ([Param(name), ...t], [`Text(p), ...url]) =>
    match_url(
      t,
      url,
      {...matches, params: [(name, Uri.pct_decode(p)), ...params]},
    )
  | ([Splat, ..._], [`Delim, ..._])
  | ([Param(_), ..._], [`Delim, ..._])
  | ([Match(_), ..._], _)
  | ([Slash, ..._], _)
  | ([_, ..._], [])
  | ([], [_, ..._]) => None
  };

let match_url = (t, url) => {
  let path =
    switch (String.index_opt(url, '?')) {
    | None => url
    | Some(i) => String.sub(url, ~pos=0, ~len=i)
    };

  let path = path |> split_slash_delim;
  match_url(t, path, {params: [], splat: []});
};
