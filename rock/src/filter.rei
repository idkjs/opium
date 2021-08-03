/** A filter is a higher order function that transforms a service into another service. */;

type t('req, 'rep, 'req', 'rep') =
  Service.t('req, 'rep) => Service.t('req', 'rep');

/** A filter is simple when it preserves the type of a service */

type simple('req, 'rep) = t('req, 'rep, 'req, 'rep);

let (>>>):
  (t('q1, 'p1, 'q2, 'p2), t('q2, 'p2, 'q3, 'p3)) => t('q1, 'p1, 'q3, 'p3);

let apply_all:
  (list(simple('req, 'rep)), Service.t('req, 'rep)) =>
  Service.t('req, 'rep);
