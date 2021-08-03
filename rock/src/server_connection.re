open Lwt.Syntax;
let (let*) = Lwt.bind;
exception Halt(Response.t);

let halt = response => raise(Halt(response));

let default_error_handler = (~request as _=?, error, start_response) => {
  open Httpaf;
  let message =
    switch (error) {
    | `Exn(_e) =>
      /* TODO: log error */
      Status.default_reason_phrase(`Internal_server_error)
    | (#Status.server_error | #Status.client_error) as error =>
      Status.default_reason_phrase(error)
    };

  let len = Int.to_string(String.length(message));
  let headers = Headers.of_list([("Content-Length", len)]);
  let body = start_response(headers);
  Body.write_string(body, message);
  Body.close_writer(body);
};

let create_error_handler = handler => {
  let error_handler = (~request=?, error, start_response) => {
    let req_headers =
      switch (request) {
      | None => Httpaf.Headers.empty
      | Some(req) => req.Httpaf.Request.headers
      };

    Lwt.async(() =>{
      let* (headers, body) = handler(req_headers, error);
      let headers =
        switch (Body.length(body)) {
        | None => headers
        | Some(l) =>
          Httpaf.Headers.add_unless_exists(
            headers,
            "Content-Length",
            Int64.to_string(l),
          )
        };

      let res_body = start_response(headers);
      let+ () =
        Lwt_stream.iter(
          s => Httpaf.Body.write_string(res_body, s),
          Body.to_stream(body),
        );
      Httpaf.Body.close_writer(res_body);
   } );
  };

  error_handler;
};

type error_handler =
  (Httpaf.Headers.t, Httpaf.Server_connection.error) =>
  Lwt.t((Httpaf.Headers.t, Body.t));

let read_httpaf_body = body =>
  Lwt_stream.from(() =>{
    let (promise, wakeup) = Lwt.wait();
    let on_eof = () => Lwt.wakeup_later(wakeup, None);
    let on_read = (buf, ~off, ~len) => {
      let b = Bytes.create(len);
      Bigstringaf.blit_to_bytes(buf, ~src_off=off, ~dst_off=0, ~len, b);
      Lwt.wakeup_later(wakeup, Some(Bytes.unsafe_to_string(b)));
    };

    Httpaf.Body.schedule_read(body, ~on_eof, ~on_read);
    promise;
  });

let httpaf_request_to_request = (~body=?, req) => {
  let headers =
    req.Httpaf.Request.headers
    |> Httpaf.Headers.to_list
    |> Httpaf.Headers.of_rev_list;

  Request.make(~headers, ~body?, req.target, req.meth);
};

let run = (server_handler, ~error_handler=?, app) => {
  let {App.middlewares, handler} = app;
  let filters = ListLabels.map(~f=m => m.Middleware.filter, middlewares);
  let service = Filter.apply_all(filters, handler);
  let request_handler = reqd =>
    Lwt.async(() =>{
      let req = Httpaf.Reqd.request(reqd);
      let req_body = Httpaf.Reqd.request_body(reqd);
      let length =
        switch (Httpaf.Request.body_length(req)) {
        | `Chunked => None
        | `Fixed(l) => Some(l)
        | `Error(_) => failwith("Bad request")
        };

      let body = {
        let stream = read_httpaf_body(req_body);
        Lwt.on_termination(Lwt_stream.closed(stream), () =>
          Httpaf.Body.close_reader(req_body)
        );
        Body.of_stream(~length?, stream);
      };

      let write_fixed_response = (~headers, f, status, body) => {
        f(reqd, Httpaf.Response.create(~headers, status), body);
        Lwt.return_unit;
      };

      let request = httpaf_request_to_request(~body, req);
      Lwt.catch(
        () => {
          let* {Response.body, headers, status, _} =
            Lwt.catch(
              () => service(request),
              fun
              | Halt(response) => Lwt.return(response)
              | exn => Lwt.fail(exn),
            );
          let {Body.length, _} = body;
          let headers =
            switch (length) {
            | None =>
              Httpaf.Headers.add_unless_exists(
                headers,
                "Transfer-Encoding",
                "chunked",
              )
            | Some(l) =>
              Httpaf.Headers.add_unless_exists(
                headers,
                "Content-Length",
                Int64.to_string(l),
              )
            };

          switch (body.content) {
          | `Empty =>
            write_fixed_response(
              ~headers,
              Httpaf.Reqd.respond_with_string,
              status,
              "",
            )
          | `String(s) =>
            write_fixed_response(
              ~headers,
              Httpaf.Reqd.respond_with_string,
              status,
              s,
            )
          | `Bigstring(b) =>
            write_fixed_response(
              ~headers,
              Httpaf.Reqd.respond_with_bigstring,
              status,
              b,
            )
          | `Stream(s) =>
            let rb =
              Httpaf.Reqd.respond_with_streaming(
                reqd,
                Httpaf.Response.create(~headers, status),
              );

            let+ () =
              Lwt_stream.iter(s => Httpaf.Body.write_string(rb, s), s);
            Httpaf.Body.flush(rb, () => Httpaf.Body.close_writer(rb));
          };
        },
        exn => {
          Httpaf.Reqd.report_exn(reqd, exn);
          Lwt.return_unit;
        },
      );
    });

  let error_handler =
    switch (error_handler) {
    | None => default_error_handler
    | Some(h) => create_error_handler(h)
    };

  server_handler(~request_handler, ~error_handler);
};
