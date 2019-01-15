open! Stdune
open Import
open OpamParserTypes

type t = opamfile

let load fn =
  Io.with_lexbuf_from_file fn ~f:(fun lb ->
    try
      OpamBaseParser.main OpamLexer.token lb (Path.to_string fn)
    with
    | OpamLexer.Error msg ->
      Errors.fail_lex lb "%s" msg
    | Parsing.Parse_error ->
      Errors.fail_lex lb "Parse error")

let get_field t name =
  List.find_map t.file_contents
    ~f:(function
      | Variable (_, var, value) when name = var ->
        Some value
      | _ -> None)

let absolutify_positions ~file_contents t =
  let open OpamParserTypes in
  let bols = ref [] in
  String.iteri file_contents ~f:(fun i ch ->
    if ch = '\n' then bols := (i + 1) :: !bols);
  let bols = Array.of_list (List.rev !bols) in
  let map_pos (fname, line, col) =
    (fname, line, bols.(line - 1) + col)
  in
  let rec map_value = function
    | Bool (pos, x) ->
      Bool (map_pos pos, x)
    | Int (pos, x) ->
      Int (map_pos pos, x)
    | String (pos, x) ->
      String (map_pos pos, x)
    | Relop (pos, x, y, z) ->
      Relop (map_pos pos, x, map_value y, map_value z)
    | Prefix_relop (pos, x, y) ->
      Prefix_relop (map_pos pos, x, map_value y)
    | Logop (pos, x, y, z) ->
      Logop (map_pos pos, x, map_value y, map_value z)
    | Pfxop (pos, x, y) ->
      Pfxop (map_pos pos, x, map_value y)
    | Ident (pos, x) ->
      Ident (map_pos pos, x)
    | List (pos, x) ->
      List (map_pos pos, List.map x ~f:map_value)
    | Group (pos, x) ->
      Group (map_pos pos, List.map x ~f:map_value)
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
