type t('req, 'rep, 'req_, 'rep_) =
  Service.t('req, 'rep) => Service.t('req_, 'rep_);
type simple('req, 'rep) = t('req, 'rep, 'req, 'rep);

let (>>>) = (f1, f2, s) => s |> f1 |> f2;
let apply_all = (filters, service) =>
  ListLabels.fold_left(filters, ~init=service, ~f=(|>));
