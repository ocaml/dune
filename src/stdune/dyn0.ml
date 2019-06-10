module Array = Dune_caml.ArrayLabels

type t =
  | Opaque
  | Unit
  | Int of int
  | Bool of bool
  | String of string
  | Bytes of bytes
  | Char of char
  | Float of float
  | Sexp of Sexp0.t
  | Option of t option
  | List of t list
  | Array of t array
  | Tuple of t list
  | Record of (string * t) list
  | Variant of string * t list
  | Map of (t * t) list
  | Set of t list

let rec pp = function
  | Opaque -> Pp.verbatim "<opaque>"
  | Unit -> Pp.verbatim "()"
  | Int i -> Pp.verbatim (string_of_int i)
  | Bool b -> Pp.verbatim (string_of_bool b)
  | String s -> Pp.verbatim s
  | Bytes b -> Pp.verbatim (Bytes.to_string b)
  | Char c -> Pp.char c
  | Float f -> Pp.verbatim (string_of_float f)
  | Sexp s -> pp_sexp s
  | Option None -> pp (Variant ("None", []))
  | Option (Some x) -> pp (Variant ("Some", [x]))
  | List x ->
    Pp.box
      (Pp.concat
         [ Pp.char '['
         ; Pp.concat_map ~sep:(Pp.seq (Pp.char ';') Pp.space) x ~f:pp
         ; Pp.char ']'
         ])
  | Array a ->
    Pp.box
      (Pp.concat
         [ Pp.verbatim "[|"
         ; Pp.concat_map ~sep:(Pp.seq (Pp.char ';') Pp.space) (Array.to_list a) ~f:pp
         ; Pp.verbatim "|]"
         ])
  | Set xs ->
    Pp.box
      (Pp.concat
         [ Pp.verbatim "set {"
         ; Pp.concat_map ~sep:(Pp.seq (Pp.char ';') Pp.space) xs ~f:pp
         ; Pp.verbatim "}"
         ])
  | Map xs ->
    Pp.box
      (Pp.concat
         [ Pp.verbatim "map {"
         ; Pp.concat_map ~sep:(Pp.seq (Pp.char ';') Pp.space) xs ~f:(fun (k, v) ->
             Pp.box
               (Pp.concat
                  [ pp k
                  ; Pp.space
                  ; Pp.verbatim ":"
                  ; Pp.space
                  ; pp v
                  ])
           )
         ; Pp.verbatim "}"
         ])
  | Tuple x ->
    Pp.box
      (Pp.concat
         [ Pp.char '('
         ; Pp.concat_map ~sep:(Pp.seq (Pp.char ',') Pp.space) x ~f:pp
         ; Pp.char ')'
         ])
  | Record fields ->
    Pp.vbox ~indent:2
      (Pp.concat
         [ Pp.char '{'
         ; Pp.concat_map ~sep:(Pp.char ';') fields ~f:(fun (f, v) ->
             Pp.concat
               [ Pp.verbatim f
               ; Pp.space
               ; Pp.char '='
               ; Pp.space
               ; Pp.box ~indent:2 (pp v)
               ]
           )
         ; Pp.char '}'
         ])
  | Variant (v, []) -> Pp.verbatim v
  | Variant (v, xs) ->
    Pp.hvbox ~indent:2
      (Pp.concat
         [ Pp.verbatim v
         ; Pp.space
         ; Pp.concat_map ~sep:(Pp.char ',') xs ~f:pp
         ])

and pp_sexp = function
  | Sexp0.Atom s -> Pp.verbatim (Escape.quote_if_needed s)
  | List [] -> Pp.verbatim "()"
  | List l ->
    Pp.box ~indent:1
      (Pp.concat
         [ Pp.char '('
         ; Pp.hvbox (Pp.concat_map l ~sep:Pp.space ~f:pp_sexp)
         ; Pp.char ')'
         ])

let pp fmt t = Pp.pp fmt (pp t)

let to_string t = Format.asprintf "%a" pp t
