include Dyn0

let rec pp = function
  | Unit -> Pp.string "()"
  | Int i -> Pp.int i
  | Bool b -> Pp.bool b
  | String s -> Pp.string s
  | Bytes b -> Pp.string (Bytes.to_string b)
  | Char c -> Pp.char c
  | Float f -> Pp.float f
  | Sexp s -> Pp.sexp s
  | Option None -> pp (Variant ("None", []))
  | Option (Some x) -> pp (Variant ("Some", [x]))
  | List x ->
    Pp.box
      [ Pp.char '['
      ; Pp.list ~sep:(Pp.seq (Pp.char ';') Pp.space) x ~f:pp
      ; Pp.char ']'
      ]
  | Array a ->
    Pp.box
      [ Pp.string "[|"
      ; Pp.list ~sep:(Pp.seq (Pp.char ';') Pp.space) (Array.to_list a) ~f:pp
      ; Pp.string "|]"
      ]
  | Set xs ->
    Pp.box
      [ Pp.string "set {"
      ; Pp.list ~sep:(Pp.seq (Pp.char ';') Pp.space) xs ~f:pp
      ; Pp.string "}"
      ]
  | Map xs ->
    Pp.box
      [ Pp.string "map {"
      ; Pp.list ~sep:(Pp.seq (Pp.char ';') Pp.space) xs ~f:(fun (k, v) ->
          Pp.box
            [ pp k
            ; Pp.space
            ; Pp.string ":"
            ; Pp.space
            ; pp v
            ]
        )
      ; Pp.string "}"
      ]
  | Tuple x ->
    Pp.box
      [ Pp.char '('
      ; Pp.list ~sep:(Pp.seq (Pp.char ',') Pp.space) x ~f:pp
      ; Pp.char ')'
      ]
  | Record fields ->
    Pp.vbox ~indent:2
      [ Pp.char '{'
      ; Pp.list ~sep:(Pp.char ';') fields ~f:(fun (f, v) ->
          Pp.concat
            [ Pp.string f
            ; Pp.space
            ; Pp.char '='
            ; Pp.space
            ; Pp.box ~indent:2 [pp v]
            ]
        )
      ; Pp.char '}'
      ]
  | Variant (v, []) -> Pp.string v
  | Variant (v, xs) ->
    Pp.hvbox ~indent:2
      [ Pp.string v
      ; Pp.space
      ; Pp.list ~sep:(Pp.char ',') xs ~f:pp
      ]

let pp fmt t = Pp.pp fmt (pp t)

let rec to_sexp =
  let open Sexp.Encoder in
  function
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

  let via_sexp f = fun x -> Sexp (f x)

  let record r = Record r

  let unknown _ = String "<unknown>"
  let opaque _ = String "<opaque>"

  let constr s args = Variant (s, args)

end

let opaque = String "<opaque>"

type dyn = t
