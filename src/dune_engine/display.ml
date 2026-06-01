open Stdune

type t =
  | Quiet
  | Short
  | Verbose

let equal a b =
  match a, b with
  | Quiet, Quiet | Short, Short | Verbose, Verbose -> true
  | _, _ -> false
;;

let repr =
  Repr.variant
    "display"
    [ Repr.case0 "Quiet" ~test:(function
        | Quiet -> true
        | Short | Verbose -> false)
    ; Repr.case0 "Short" ~test:(function
        | Short -> true
        | Quiet | Verbose -> false)
    ; Repr.case0 "Verbose" ~test:(function
        | Verbose -> true
        | Quiet | Short -> false)
    ]
;;

let to_dyn = Repr.to_dyn repr
