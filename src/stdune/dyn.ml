module Array  = Dune_caml.ArrayLabels
module List   = Dune_caml.ListLabels
module String = Dune_caml.StringLabels
module Bytes  = Dune_caml.Bytes

type t =
  | Opaque
  | Unit
  | Int of int
  | Bool of bool
  | String of string
  | Bytes of bytes
  | Char of char
  | Float of float
  | Option of t option
  | List of t list
  | Array of t array
  | Tuple of t list
  | Record of (string * t) list
  | Variant of string * t list
  | Map of (t * t) list
  | Set of t list

let unsnoc l = match List.rev l with
  | last :: before_last -> Some (List.rev before_last, last)
  | [] -> None

let string_in_ocaml_syntax str =
  let is_space = function
    | ' ' ->
      (* don't need to handle tabs because those are already escaped *)
      true
    | _ -> false
  in
  let escape_protect_first_space s =
    let first_char =
      if String.length s > 0 && is_space s.[0]
      then "\\"
      else
        " "
    in
    first_char ^  String.escaped s
  in
  (* CR-someday aalekseyev: should use the method from
     [Dune_lang.prepare_formatter] so that the formatter can fit multiple
     lines on one line. *)
  match String_split.split ~on:'\n' str with
  | [] -> assert false
  | first :: rest -> match unsnoc rest with
    | None -> Pp.verbatim (Printf.sprintf "%S" first)
    | Some (middle, last) ->
      Pp.vbox (
        Pp.concat ~sep:Pp.newline (
          List.map ~f:Pp.verbatim (
            ("\"" ^ (String.escaped first) ^ "\\n\\") ::
            List.map middle ~f:(fun s ->
              (escape_protect_first_space s) ^ "\\n\\"
            ) @
            [ escape_protect_first_space last ^ "\"" ]
          )))

let rec pp = function
  | Opaque -> Pp.verbatim "<opaque>"
  | Unit -> Pp.verbatim "()"
  | Int i -> Pp.verbatim (string_of_int i)
  | Bool b -> Pp.verbatim (string_of_bool b)
  | String s -> string_in_ocaml_syntax s
  | Bytes b -> string_in_ocaml_syntax (Bytes.to_string b)
  | Char c -> Pp.char c
  | Float f -> Pp.verbatim (string_of_float f)
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
         ; Pp.concat_map  fields
             ~sep:(Pp.seq (Pp.char ';') Pp.space)
             ~f:(fun (f, v) ->
               Pp.box ~indent:2 (Pp.concat
                                   [ Pp.verbatim f
                                   ; Pp.space
                                   ; Pp.char '='
                                   ; Pp.space
                                   ; pp v
                                   ])
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

let pp fmt t = Pp.render_ignore_tags fmt (pp t)

let to_string t = Format.asprintf "%a" pp t

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
  let option f = fun x ->
    Option (match x with
      | None -> None
      | Some x -> Some (f x))

  let record r = Record r

  let unknown _ = String "<unknown>"
  let opaque _ = String "<opaque>"

  let constr s args = Variant (s, args)

end

let opaque = String "<opaque>"

type dyn = t

let hash = Dune_caml.Hashtbl.hash
let compare x y = Ordering.of_int (compare x y)
