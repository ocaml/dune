type t =
  | Ignore
  | Produce

let to_dyn = function
  | Ignore -> Dyn.variant "Ignore" []
  | Produce -> Dyn.variant "Produce" []
;;
