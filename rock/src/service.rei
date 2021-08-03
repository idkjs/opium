/** A service is a function that returns its result asynchronously. */;

type t('req, 'rep) = 'req => Lwt.t('rep);
