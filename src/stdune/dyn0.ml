module Array = Dune_caml.ArrayLabels
module List = Dune_caml.ListLabels

module String = Dune_caml.StringLabels

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
