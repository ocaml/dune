include Dyn0

let rec to_sexp =
  let open Sexp.Encoder in
  function
  | Opaque -> Sexp.Atom "<opaque>"
  | Unit -> unit ()
  | Int i -> int i
  | Bool b -> bool b
  | String s -> string s
  | Bytes s -> string (Bytes.to_string s)
  | Char c -> char c
  | Float f -> float f
  | Sexp s -> s
  | Option o -> option to_sexp o
  | List l -> list to_sexp l
  | Array a -> array to_sexp a
  | Map xs -> list (pair to_sexp to_sexp) xs
  | Set xs -> list to_sexp xs
  | Tuple t -> list to_sexp t
  | Record fields ->
    List.map fields ~f:(fun (field, f) -> (field, to_sexp f))
    |> record
  | Variant (s, []) -> string s
  | Variant (s, xs) -> constr s (List.map xs ~f:to_sexp)

module Encoder = struct

  type dyn = t

  type 'a t = 'a -> dyn

  let unit = fun () -> Unit
  let char = fun x -> Char x
  let string = fun x -> String x
  let int = fun x -> Int x
  let float = fun x -> Float x
  let bool = fun x -> Bool x
  let pair f g = fun (x, y) -> Tuple [f x; g y]
  let triple f g h = fun (x, y, z) -> Tuple [f x; g y; h z]
  let list f = fun l -> List (List.map ~f l)
  let array f = fun a -> Array (Array.map ~f a)
  let option f = fun x -> Option (Option.map ~f x)

  let record r = Record r

  let unknown _ = String "<unknown>"
  let opaque _ = String "<opaque>"

  let constr s args = Variant (s, args)

end

let opaque = String "<opaque>"

type dyn = t

let hash = Dune_caml.Hashtbl.hash
let compare x y = Ordering.of_int (compare x y)
