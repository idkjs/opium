type t = {
  filter: Filter.simple(Request.t, Response.t),
  name: string,
};

let create = (~filter, ~name) => {filter, name};
let apply = ({filter, _}, handler) => filter(handler);
