open! Stdune
open Import
open OpamParserTypes

type t = opamfile

let parse (lb : Lexing.lexbuf) =
  try
    OpamBaseParser.main OpamLexer.token lb lb.lex_curr_p.pos_fname
  with
  | OpamLexer.Error msg ->
    Errors.fail_lex lb "%s" msg
  | Parsing.Parse_error ->
    Errors.fail_lex lb "Parse error"

let of_string ~path s =
  let lb = Lexing.from_string s in
  lb.lex_curr_p <-
    { pos_fname = Path.to_string path
    ; pos_lnum  = 1
    ; pos_bol   = 0
    ; pos_cnum  = 0
    };
  parse lb

let load fn =
  Io.with_lexbuf_from_file fn ~f:parse

let get_field t name =
  List.find_map t.file_contents
    ~f:(function
      | Variable (_, var, value) when name = var ->
        Some value
      | _ -> None)

let absolutify_positions ~file_contents t =
  let open OpamParserTypes in
  let bols = ref [0] in
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

let nopos : OpamParserTypes.pos = ("",0,0) (* Null position *)

module Mutator = struct
  open OpamParserTypes

  type t = opamfile_item list -> opamfile_item list

  let (>>>) : t -> t -> t = fun x y z -> y (x z)

  let fixup : t = List.map ~f:(function
    | Variable (x,y,String (pos,z)) ->
      let fixed =
        if String.length z > 0 && z.[0] = '\n'
        then String.sub z ~pos:1 ~len:(String.length z - 1)
        else z
      in
      Variable (x,y,String (pos,fixed))
    | y -> y)

  let _remove_var : string -> t =
    fun str -> List.filter ~f:(function
      | Variable (_, v, _) when v=str -> false
      | _ -> true)

  let add_var : string -> OpamParserTypes.value -> t = fun var value l ->
    (Variable (nopos, var, value))::l

  let remap x f =
    List.filter_map ~f:(function
      | Variable (_, v, y) when v = x -> begin
          match f (Some y) with
          | Some y' -> Some (Variable (nopos, v, y'))
          | None -> None
        end
      | z -> Some z)

  let binding_present x =
    List.exists ~f:(function
      | Variable (_, v, _) when v = x -> true
      | _ -> false)

  let _map_var x f zs =
    if binding_present x zs
    then remap x f zs
    else begin
      match f None with
      | Some y -> (Variable (nopos, x, y))::zs
      | None -> zs
    end

  let set_var x y zs =
    if binding_present x zs
    then remap x (fun _ -> Some y) zs
    else add_var x y zs

  let mkstring x = String (nopos, x)
  let mklist f xs = List (nopos, List.map ~f xs)

  let set_string x y = set_var x (mkstring y)

  let set_list x conv l = set_var x (mklist conv l)
  let id x = x

  let opt opt f : t = match opt with | None -> id | Some x -> f x
  let list l f : t = match l with [] -> id | xs -> f xs

  let apply t opamfile =
    {
      opamfile with
      file_contents = t opamfile.file_contents
    }
end
