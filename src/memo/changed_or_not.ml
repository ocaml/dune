open! Import

type 'cycle t =
  | Unchanged
  | Changed
  | Cancelled of { dependency_cycle : 'cycle }

let combine x y =
  match x, y with
  | Cancelled _, Cancelled _ ->
    (* This is the only non-commutative case: we prefer the dependency cycle [x] here. We
       could combine the two cycles into a set of cycles but it doesn't seems worth it. *)
    x
  | Cancelled _, _ -> x
  | _, Cancelled _ -> y
  | Changed, _ | _, Changed -> Changed
  | Unchanged, Unchanged -> Unchanged
;;
