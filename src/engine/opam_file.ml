open! Stdune
open Import
open OpamParserTypes

type t = opamfile

let parse_gen entry (lb : Lexing.lexbuf) =
  try entry OpamLexer.token lb with
  | OpamLexer.Error msg ->
    User_error.raise ~loc:(Loc.of_lexbuf lb) [ Pp.text msg ]
  | Parsing.Parse_error ->
    User_error.raise ~loc:(Loc.of_lexbuf lb) [ Pp.text "Parse error" ]

let parse =
  parse_gen (fun lexer lexbuf ->
      OpamBaseParser.main lexer lexbuf lexbuf.Lexing.lex_curr_p.pos_fname)

let parse_value = parse_gen OpamBaseParser.value

let load fn = Io.with_lexbuf_from_file fn ~f:parse

let get_field t name =
  List.find_map t.file_contents ~f:(function
    | Variable (_, var, value) when name = var -> Some value
    | _ -> None)

let absolutify_positions ~file_contents t =
  let open OpamParserTypes in
  let bols = ref [ 0 ] in
  String.iteri file_contents ~f:(fun i ch ->
      if ch = '\n' then bols := (i + 1) :: !bols);
  let bols = Array.of_list (List.rev !bols) in
  let map_pos (fname, line, col) = (fname, line, bols.(line - 1) + col) in
  let rec map_value = function
    | Bool (pos, x) -> Bool (map_pos pos, x)
    | Int (pos, x) -> Int (map_pos pos, x)
    | String (pos, x) -> String (map_pos pos, x)
    | Relop (pos, x, y, z) -> Relop (map_pos pos, x, map_value y, map_value z)
    | Prefix_relop (pos, x, y) -> Prefix_relop (map_pos pos, x, map_value y)
    | Logop (pos, x, y, z) -> Logop (map_pos pos, x, map_value y, map_value z)
    | Pfxop (pos, x, y) -> Pfxop (map_pos pos, x, map_value y)
    | Ident (pos, x) -> Ident (map_pos pos, x)
    | List (pos, x) -> List (map_pos pos, List.map x ~f:map_value)
    | Group (pos, x) -> Group (map_pos pos, List.map x ~f:map_value)
    | Option (pos, x, y) ->
      Option (map_pos pos, map_value x, List.map y ~f:map_value)
    | Env_binding (pos, x, y, z) ->
      Env_binding (map_pos pos, map_value x, y, map_value z)
  in
  let rec map_section s =
    { s with section_items = List.map s.section_items ~f:map_item }
  and map_item = function
    | Section (pos, s) -> Section (map_pos pos, map_section s)
    | Variable (pos, s, v) -> Variable (map_pos pos, s, map_value v)
  in
  { file_contents = List.map t.file_contents ~f:map_item
  ; file_name = t.file_name
  }

let nopos : OpamParserTypes.pos = ("", 0, 0) (* Null position *)

let existing_variables t =
  List.fold_left ~init:String.Set.empty t.file_contents ~f:(fun acc l ->
      match l with
      | Section (_, _) -> acc
      | Variable (_, var, _) -> String.Set.add acc var)

module Create = struct
  let string s = String (nopos, s)

  let list f xs = List (nopos, List.map ~f xs)

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
        (* This [lazy] and the mutable table below are pure and hence safe. *)
        lazy
          (let table = Table.create (module String) (Array.length fields) in
           Array.iteri fields ~f:(fun i field -> Table.add_exn table field i);
           table)
      in
      fun key -> Table.find (Lazy.force table) key
    in
    fun vars ->
      List.stable_sort vars ~compare:(fun (x, _) (y, _) ->
          match (normal_field_order x, normal_field_order y) with
          | Some x, Some y -> Int.compare x y
          | Some _, None -> Lt
          | None, Some _ -> Gt
          | None, None -> Eq)

  let of_bindings vars ~file =
    let file_contents =
      List.map vars ~f:(fun (var, value) -> Variable (nopos, var, value))
    in
    let file_name = Path.to_string file in
    { file_contents; file_name }
end
