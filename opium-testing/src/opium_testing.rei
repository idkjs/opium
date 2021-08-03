/** This module provides helpers to easily test Opium applications with Alcotest. */;

/** {3 [Testable]} */;

/** Collection of [Alcotest] testables for [Opium] types. */

module Testable: {
  /** An {!Alcotest.testable} for {!Opium.Status.t} instances. */

  let status: Alcotest.testable(Opium.Status.t);

  /** An {!Alcotest.testable} for {!Opium.Method.t} instances. */

  let meth: Alcotest.testable(Opium.Method.t);

  /** An {!Alcotest.testable} for {!Opium.Version.t} instances. */

  let version: Alcotest.testable(Opium.Version.t);

  /** An {!Alcotest.testable} for {!Opium.Body.t} instances. */

  let body: Alcotest.testable(Opium.Body.t);

  /** An {!Alcotest.testable} for {!Opium.Request.t} instances. */

  let request: Alcotest.testable(Opium.Request.t);

  /** An {!Alcotest.testable} for {!Opium.Response.t} instances. */

  let response: Alcotest.testable(Opium.Response.t);

  /** An {!Alcotest.testable} for {!Opium.Cookie.t} instances. */

  let cookie: Alcotest.testable(Opium.Cookie.t);
};

/** {3 [handle_request]} */;

/** [handle_request app request response] processes a request [request] with the given
    Opium application [app].

    It processes the request the same [Opium.Server_connection.run] would and returns the
    generated response. */

let handle_request: (Opium.App.t, Opium.Request.t) => Lwt.t(Opium.Response.t);

/** {3 [check_status]} */;

/** [check_status ?msg t1 t2] checks that the status [t1] and [t2] are equal. */

let check_status: (~msg: string=?, Opium.Status.t, Opium.Status.t) => unit;

/** {3 [check_status']} */;

/** [check_status' ?msg t1 t2] checks that the status [t1] and [t2] are equal.

    This is a labeled variant of {!check_status} */

let check_status':
  (~msg: string=?, ~expected: Opium.Status.t, ~actual: Opium.Status.t) => unit;

/** {3 [check_meth]} */;

/** [check_meth ?msg t1 t2] checks that the method [t1] and [t2] are equal. */

let check_meth: (~msg: string=?, Opium.Method.t, Opium.Method.t) => unit;

/** {3 [check_meth']} */;

/** [check_meth' ?msg t1 t2] checks that the method [t1] and [t2] are equal.

    This is a labeled variant of {!check_meth} */

let check_meth':
  (~msg: string=?, ~expected: Opium.Method.t, ~actual: Opium.Method.t) => unit;

/** {3 [check_version]} */;

/** [check_version ?msg t1 t2] checks that the version [t1] and [t2] are equal. */

let check_version: (~msg: string=?, Opium.Version.t, Opium.Version.t) => unit;

/** {3 [check_version']} */;

/** [check_version' ?msg t1 t2] checks that the version [t1] and [t2] are equal.

    This is a labeled variant of {!check_version} */

let check_version':
  (~msg: string=?, ~expected: Opium.Version.t, ~actual: Opium.Version.t) =>
  unit;

/** {3 [check_body]} */;

/** [check_body ?msg t1 t2] checks that the body [t1] and [t2] are equal. */

let check_body: (~msg: string=?, Opium.Body.t, Opium.Body.t) => unit;

/** {3 [check_body']} */;

/** [check_body' ?msg t1 t2] checks that the body [t1] and [t2] are equal.

    This is a labeled variant of {!check_body} */

let check_body':
  (~msg: string=?, ~expected: Opium.Body.t, ~actual: Opium.Body.t) => unit;

/** {3 [check_request]} */;

/** [check_request ?msg t1 t2] checks that the request [t1] and [t2] are equal. */

let check_request: (~msg: string=?, Opium.Request.t, Opium.Request.t) => unit;

/** {3 [check_request']} */;

/** [check_request' ?msg t1 t2] checks that the request [t1] and [t2] are equal.

    This is a labeled variant of {!check_request} */

let check_request':
  (~msg: string=?, ~expected: Opium.Request.t, ~actual: Opium.Request.t) =>
  unit;

/** {3 [check_response]} */;

/** [check_response ?msg t1 t2] checks that the response [t1] and [t2] are equal. */

let check_response:
  (~msg: string=?, Opium.Response.t, Opium.Response.t) => unit;

/** {3 [check_response']} */;

/** [check_response' ?msg t1 t2] checks that the response [t1] and [t2] are equal.

    This is a labeled variant of {!check_response} */

let check_response':
  (~msg: string=?, ~expected: Opium.Response.t, ~actual: Opium.Response.t) =>
  unit;

/** {3 [check_cookie]} */;

/** [check_cookie ?msg t1 t2] checks that the cookie [t1] and [t2] are equal. */

let check_cookie: (~msg: string=?, Opium.Cookie.t, Opium.Cookie.t) => unit;

/** {3 [check_cookie']} */;

/** [check_cookie' ?msg t1 t2] checks that the cookie [t1] and [t2] are equal.

    This is a labeled variant of {!check_cookie} */

let check_cookie':
  (~msg: string=?, ~expected: Opium.Cookie.t, ~actual: Opium.Cookie.t) => unit;

/** {3 [check_body_contains]} */;

/** [check_body_contains ?msg s t] checks that the body [t] contains the string [s]. */

let check_body_contains:
  (~msg: string=?, string, Opium.Body.t) => Lwt.t(unit);
