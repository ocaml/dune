open Import

type t =
  | Ignore
  | Produce

let repr =
  Repr.variant
    "corrections"
    [ Repr.case0 "Ignore" ~test:(function
        | Ignore -> true
        | Produce -> false)
    ; Repr.case0 "Produce" ~test:(function
        | Produce -> true
        | Ignore -> false)
    ]
;;

let to_dyn = Repr.to_dyn repr
