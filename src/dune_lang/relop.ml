open Stdune

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
let compare a b : Ordering.t =
  match a, b with
  | Eq, Eq -> Eq
  | Eq, _ -> Lt
  | _, Eq -> Gt
  | Gte, Gte -> Eq
  | Gte, _ -> Lt
  | _, Gte -> Gt
  | Lte, Lte -> Eq
  | Lte, _ -> Lt
  | _, Lte -> Gt
  | Gt, Gt -> Eq
  | Gt, _ -> Lt
  | _, Gt -> Gt
  | Lt, Lt -> Eq
  | Lt, _ -> Lt
  | _, Lt -> Gt
  | Neq, Neq -> Eq
;;

let equal a b = Ordering.is_eq (compare a b)
let map = [ "=", Eq; ">=", Gte; "<=", Lte; ">", Gt; "<", Lt; "<>", Neq ]
let to_dyn t = Dyn.variant (fst (List.find_exn ~f:(fun (_, op) -> equal t op) map)) []

let to_string x =
  let f (_, op) = equal x op in
  (* Assumes the [map] is complete, so exception is impossible *)
  List.find_exn ~f map |> fst
;;

let encode x = to_string x |> Dune_sexp.Encoder.string

let eval t (x : Ordering.t) =
  match t, x with
  | (Eq | Gte | Lte), Eq | (Neq | Lt | Lte), Lt | (Neq | Gt | Gte), Gt -> true
  | _, _ -> false
;;
