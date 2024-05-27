open Stdune
open OpamParserTypes.FullPos

type t = opamfile

let loc_of_opam_pos
  ({ filename; start = start_line, start_column; stop = stop_line, stop_column } :
    OpamParserTypes.FullPos.pos)
  =
  let start =
    { Lexing.pos_fname = filename
    ; pos_lnum = start_line
    ; pos_bol = 0
    ; pos_cnum = start_column
    }
  in
  let stop =
    { Lexing.pos_fname = filename
    ; pos_lnum = stop_line
    ; pos_bol = 0
    ; pos_cnum = stop_column
    }
  in
  Loc.create ~start ~stop
;;

let read_from_string_exn ~contents path =
  try
    OpamFile.OPAM.read_from_string
      contents
      ~filename:(Path.to_absolute_filename path |> OpamFilename.raw |> OpamFile.make)
  with
  | OpamPp.Bad_version (_, message) ->
    User_error.raise
      ~loc:(Loc.in_file path)
      [ Pp.text "unexpected version"; Pp.text message ]
  | OpamPp.Bad_format (pos, message) ->
    let loc =
      match pos with
      | None -> Loc.in_file path
      | Some pos -> loc_of_opam_pos pos
    in
    User_error.raise ~loc [ Pp.text "unable to parse opam file"; Pp.text message ]
;;

let parse_gen entry (lb : Lexing.lexbuf) =
  try entry OpamLexer.token lb with
  | OpamLexer.Error msg -> User_error.raise ~loc:(Loc.of_lexbuf lb) [ Pp.text msg ]
  | Parsing.Parse_error ->
    User_error.raise ~loc:(Loc.of_lexbuf lb) [ Pp.text "Parse error" ]
;;

let parse =
  parse_gen (fun lexer (lexbuf : Lexing.lexbuf) ->
    OpamBaseParser.main lexer lexbuf lexbuf.lex_curr_p.pos_fname)
;;

let parse_value = parse_gen OpamBaseParser.value

let get_field t name =
  List.find_map t.file_contents ~f:(fun value ->
    match value.pelem with
    | Variable (var, value) when var.pelem = name -> Some value
    | _ -> None)
;;

let absolutify_positions ~file_contents t =
  let bols = ref [ 0 ] in
  String.iteri file_contents ~f:(fun i ch -> if ch = '\n' then bols := (i + 1) :: !bols);
  let bols = Array.of_list (List.rev !bols) in
  let map_pos { filename; start = start_line, start_col; stop = stop_line, stop_col } =
    let start = start_line, bols.(start_line - 1) + start_col in
    let stop = stop_line, bols.(stop_line - 1) + stop_col in
    { filename; start; stop }
  in
  let repos pelem pos = { pelem; pos = map_pos pos } in
  let rec map_value = function
    | { pelem = Bool x; pos } -> repos (Bool x) pos
    | { pelem = Int x; pos } -> repos (Int x) pos
    | { pelem = String x; pos } -> repos (String x) pos
    | { pelem = Relop (x, y, z); pos } -> repos (Relop (x, map_value y, map_value z)) pos
    | { pelem = Prefix_relop (x, y); pos } -> repos (Prefix_relop (x, map_value y)) pos
    | { pelem = Logop (x, y, z); pos } -> repos (Logop (x, map_value y, map_value z)) pos
    | { pelem = Pfxop (x, y); pos } -> repos (Pfxop (x, map_value y)) pos
    | { pelem = Ident x; pos } -> repos (Ident x) pos
    | { pelem = List xs; pos } ->
      let pelem = List.map xs.pelem ~f:map_value in
      let xs = { pelem; pos = map_pos xs.pos } in
      repos (List xs) pos
    | { pelem = Group xs; pos } ->
      let pelem = List.map xs.pelem ~f:map_value in
      let xs = { pelem; pos = map_pos xs.pos } in
      repos (Group xs) pos
    | { pelem = Option (x, ys); pos } ->
      let pelem = List.map ys.pelem ~f:map_value in
      let ys = { pelem; pos = map_pos ys.pos } in
      repos (Option (map_value x, ys)) pos
    | { pelem = Env_binding (x, y, z); pos } ->
      repos (Env_binding (map_value x, y, map_value z)) pos
  in
  let rec map_section s =
    let { pelem; pos } = s.section_items in
    let pelem = List.map pelem ~f:map_item in
    let pos = map_pos pos in
    let section_items = { pelem; pos } in
    { s with section_items }
  and map_item = function
    | { pelem = Section s; pos } -> repos (Section (map_section s)) pos
    | { pelem = Variable (s, v); pos } -> repos (Variable (s, map_value v)) pos
  in
  { file_contents = List.map t.file_contents ~f:map_item; file_name = t.file_name }
;;

let nopos : pos = { filename = ""; start = 0, 0; stop = 0, 0 }
(* Null position *)

let existing_variables t =
  List.fold_left ~init:String.Set.empty t.file_contents ~f:(fun acc l ->
    match l.pelem with
    | Section _ -> acc
    | Variable (var, _) -> String.Set.add acc var.pelem)
;;

module Create = struct
  let string s = { pelem = String s; pos = nopos }

  let list f xs =
    let elems = { pelem = List.map ~f xs; pos = nopos } in
    { pelem = List elems; pos = nopos }
  ;;

  let string_list xs = list string xs

  let normalise_field_order =
    let normal_field_order =
      let fields =
        [| (* Extracted from opam/src/format/opamFile.ml *)
           "opam-version"
         ; "name"
         ; "version"
         ; "synopsis"
         ; "description"
         ; "maintainer"
         ; "authors"
         ; "author"
         ; "license"
         ; "tags"
         ; "homepage"
         ; "doc"
         ; "bug-reports"
         ; "depends"
         ; "depopts"
         ; "conflicts"
         ; "conflict-class"
         ; "available"
         ; "flags"
         ; "setenv"
         ; "build"
         ; "run-test"
         ; "install"
         ; "remove"
         ; "substs"
         ; "patches"
         ; "build-env"
         ; "features"
         ; "messages"
         ; "post-messages"
         ; "depexts"
         ; "libraries"
         ; "syntax"
         ; "dev-repo"
         ; "pin-depends"
         ; "extra-files"
        |]
      in
      let table =
        lazy
          (let table = Table.create (module String) (Array.length fields) in
           Array.iteri fields ~f:(fun i field -> Table.add_exn table field i);
           table)
      in
      fun key -> Table.find (Lazy.force table) key
    in
    fun vars ->
      List.stable_sort vars ~compare:(fun (x, _) (y, _) ->
        match normal_field_order x, normal_field_order y with
        | Some x, Some y -> Int.compare x y
        | Some _, None -> Lt
        | None, Some _ -> Gt
        | None, None -> Eq)
  ;;

  let of_bindings vars ~file =
    let file_contents =
      List.map vars ~f:(fun (var, value) ->
        let var = { pelem = var; pos = nopos } in
        { pelem = Variable (var, value); pos = nopos })
    in
    let file_name = Path.to_string file in
    { file_contents; file_name }
  ;;
end
