include Dyn0

let rec to_sexp : t -> Sexp0.t = function
  | Opaque -> Atom "<opaque>"
  | Unit -> List []
  | Int i -> Atom (string_of_int i)
  | Bool b -> Atom (string_of_bool b)
  | String s -> Atom s
  | Bytes s -> Atom (Stdlib.Bytes.to_string s)
  | Char c -> Atom (Stdlib.String.make 1 c)
  | Float f -> Atom (string_of_float f)
  | Sexp s -> s
  | Option o ->
    List (match o with
      | None -> []
      | Some x -> [to_sexp x])
  | List l -> List (List.map l ~f:to_sexp)
  | Array a -> List (Array.to_list a |> List.map ~f:to_sexp)
  | Map xs -> List (List.map xs ~f:(fun (k, v) ->
    Sexp0.List [to_sexp k; to_sexp v]))
  | Set xs -> List (List.map xs ~f:to_sexp)
  | Tuple t -> List (List.map t ~f:to_sexp)
  | Record fields ->
    List (List.map fields ~f:(fun (field, f) ->
      Sexp0.List [Atom field; to_sexp f]))
  | Variant (s, []) -> Atom s
  | Variant (s, xs) -> List (Atom s :: List.map xs ~f:to_sexp)

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
