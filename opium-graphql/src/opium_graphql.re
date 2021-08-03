module Option = {
  include Option;

  let bind = (t, ~f) =>
    switch (t) {
    | None => None
    | Some(x) => f(x)
    };

  let map = (t, ~f) => bind(t, ~f=x => Some(f(x)));

  let first_some = (t, t') =>
    switch (t) {
    | None => t'
    | Some(_) => t
    };
};

module Params = {
  type t = {
    query: option(string),
    variables: option(list((string, Yojson.Basic.t))),
    operation_name: option(string),
  };

  let empty = {query: None, variables: None, operation_name: None};

  let of_uri_exn = uri => {
    let variables =
      Uri.get_query_param(uri, "variables")
      |> Option.map(~f=Yojson.Basic.from_string)
      |> Option.map(~f=Yojson.Basic.Util.to_assoc);

    {
      query: Uri.get_query_param(uri, "query"),
      variables,
      operation_name: Uri.get_query_param(uri, "operationName"),
    };
  };

  let of_json_body_exn = body =>
    if (body == "") {
      empty;
    } else {
      let json = Yojson.Basic.from_string(body);
      {
        query:
          Yojson.Basic.Util.(
            json |> member("query") |> to_option(to_string)
          ),
        variables:
          Yojson.Basic.Util.(
            json |> member("variables") |> to_option(to_assoc)
          ),
        operation_name:
          Yojson.Basic.Util.(
            json |> member("operationName") |> to_option(to_string)
          ),
      };
    };

  let of_graphql_body = body => {
    query: Some(body),
    variables: None,
    operation_name: None,
  };

  let merge = (t, t') => {
    query: Option.first_some(t.query, t'.query),
    variables: Option.first_some(t.variables, t'.variables),
    operation_name: Option.first_some(t.operation_name, t'.operation_name),
  };

  let post_params_exn = (req, body) => {
    let headers = req.Opium.Request.headers;
    switch (Httpaf.Headers.get(headers, "Content-Type")) {
    | Some("application/graphql") => of_graphql_body(body)
    | Some("application/json") => of_json_body_exn(body)
    | _ => empty
    };
  };

  let of_req_exn = (req, body) => {
    let get_params = req.Opium.Request.target |> Uri.of_string |> of_uri_exn;
    let post_params = post_params_exn(req, body);
    merge(get_params, post_params);
  };

  let extract = (req, body) =>
    try({
      let params = of_req_exn(req, body);
      switch (params.query) {
      | Some(query) =>
        [@implicit_arity]
        Ok(
          query,
          (
            params.variables :>
              option(list((string, Graphql_parser.const_value)))
          ),
          params.operation_name,
        )
      | None => Error("Must provide query string")
      };
    }) {
    | Yojson.Json_error(msg) => Error(msg)
    };
};

module Schema = Graphql_lwt.Schema;

let basic_to_safe = json =>
  json |> Yojson.Basic.to_string |> Yojson.Safe.from_string;

let execute_query = (ctx, schema, variables, operation_name, query) =>
  switch (Graphql_parser.parse(query)) {
  | Ok(doc) => Schema.execute(schema, ctx, ~variables?, ~operation_name?, doc)
  | Error(e) => Lwt.return(Error(`String(e)))
  };

let execute_request = (schema, ctx, req) => {
  open Lwt.Syntax;
  let* body_string = Opium.Body.to_string(req.Opium.Request.body);
  switch (Params.extract(req, body_string)) {
  | Error(err) =>
    Opium.Response.of_plain_text(~status=`Bad_request, err) |> Lwt.return
  | [@implicit_arity] Ok(query, variables, operation_name) =>
    let+ result =
      execute_query(ctx, schema, variables, operation_name, query);
    switch (result) {
    | Ok(`Response(data)) =>
      data |> basic_to_safe |> Opium.Response.of_json(~status=`OK)
    | Ok(`Stream(stream)) =>
      Graphql_lwt.Schema.Io.Stream.close(stream);
      let body = "Subscriptions are only supported via websocket transport";
      Opium.Response.of_plain_text(~status=`Bad_request, body);
    | Error(err) =>
      err |> basic_to_safe |> Opium.Response.of_json(~status=`Bad_request)
    };
  };
};

let make_handler:
  type a.
    (~make_context: Rock.Request.t => a, Graphql_lwt.Schema.schema(a)) =>
    Rock.Handler.t =
  (~make_context, schema, req) =>
    switch (req.Opium.Request.meth) {
    | `GET =>
      if (Httpaf.Headers.get(req.Opium.Request.headers, "Connection")
          == Some("Upgrade")
          && Httpaf.Headers.get(req.Opium.Request.headers, "Upgrade")
          == Some("websocket")) {
        /* TODO: Add subscription support when there is a good solution for websockets with
           Httpaf */
        Opium.Response.of_plain_text(
          ~status=`Internal_server_error,
          "Subscriptions are not supported (yet)",
        )
        |> Lwt.return;
      } else {
        execute_request(schema, make_context(req), req);
      }
    | `POST => execute_request(schema, make_context(req), req)
    | _ => Opium.Response.make(~status=`Method_not_allowed, ()) |> Lwt.return
    };

let graphiql_etag =
  Asset.read("graphiql.html")
  |> Option.get
  |> Cstruct.of_string
  |> Mirage_crypto.Hash.digest(`MD5)
  |> Cstruct.to_string
  |> Base64.encode_exn;

let make_graphiql_handler = (~graphql_endpoint, req) => {
  let accept_html =
    switch (Httpaf.Headers.get(req.Opium.Request.headers, "accept")) {
    | None => false
    | Some(s) => List.mem("text/html", String.split_on_char(',', s))
    };

  let h =
    Opium.Handler.serve(
      ~etag=graphiql_etag, ~mime_type="text/html; charset=utf-8", () =>
      switch (Asset.read("graphiql.html")) {
      | None => Lwt.return_error(`Internal_server_error)
      | Some(body) =>
        let regexp = Str.regexp_string("%%GRAPHQL_API%%");
        let body = Str.global_replace(regexp, graphql_endpoint, body);
        Lwt.return_ok(Opium.Body.of_string(body));
      }
    );

  if (accept_html) {
    h(req);
  } else {
    Opium.Response.of_plain_text(
      ~status=`Bad_request,
      "Clients must accept text/html",
    )
    |> Lwt.return;
  };
};
