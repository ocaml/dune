type t = (int, string) Type_eq.t

(* The purpose of [dummy] is to get the definition of [unreachable_code] that
  OCaml 4.02 would accept without having to write "assert false".

   The problem is that 4.02 doesn't have refutation branches, so we turn the
   "nullary" pattern-match into an equivalent one with one branch. *)
type ('a, 'b, 'c) dummy =
  | No of 'c
  | Eq : ('a, 'a, 'c) dummy

let _f x = No x

let to_dummy : type a b. (a, b) Type_eq.t -> (a, b, 'c) dummy =
 fun Type_eq.T -> Eq

let unreachable_code (t : t) =
  match to_dummy t with
  | No c -> c
