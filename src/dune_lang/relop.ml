open Import

type t =
  | Eq
  | Gte
  | Lte
  | Gt
  | Lt
  | Neq

(* Define an arbitrary ordering on [t] to allow a package constraint to be
   used as the key of a map or set. The order from lowest to highest is:
   [Eq, Gte, Lte, Gt, Lt, Neq] *)
let repr =
  Repr.variant
    "relop"
    [ Repr.case0 "=" ~test:(function
        | Eq -> true
        | _ -> false)
    ; Repr.case0 ">=" ~test:(function
        | Gte -> true
        | _ -> false)
    ; Repr.case0 "<=" ~test:(function
        | Lte -> true
        | _ -> false)
    ; Repr.case0 ">" ~test:(function
        | Gt -> true
        | _ -> false)
    ; Repr.case0 "<" ~test:(function
        | Lt -> true
        | _ -> false)
    ; Repr.case0 "<>" ~test:(function
        | Neq -> true
        | _ -> false)
    ]
;;

let equal, compare = Repr.make_compare repr
let map = [ "=", Eq; ">=", Gte; "<=", Lte; ">", Gt; "<", Lt; "<>", Neq ]
let to_dyn = Repr.to_dyn repr

let to_string x =
  let f (_, op) = equal x op in
  (* Assumes the [map] is complete, so exception is impossible *)
  List.find_exn ~f map |> fst
;;

let encode x = to_string x |> Encoder.string

let eval t (x : Ordering.t) =
  match t, x with
  | (Eq | Gte | Lte), Eq | (Neq | Lt | Lte), Lt | (Neq | Gt | Gte), Gt -> true
  | _, _ -> false
;;
