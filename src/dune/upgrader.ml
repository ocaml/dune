open! Stdune
open Import
open Upgrader_common

(* Return a mapping [Path.t -> Dune_lang.Ast.t list] containing [path] and all
   the files in includes, recursiverly *)
let scan_included_files path =
  let files = ref Path.Source.Map.empty in
  let rec iter path =
    if not (Path.Source.Map.mem !files path) then (
      let s = Io.read_file (Path.source path) in
      let csts =
        Dune_lang.Parser.parse_string s
          ~fname:(Path.Source.to_string path)
          ~lexer:Jbuild_support.Lexer.token ~mode:Cst
        |> List.map ~f:(Dune_lang.Cst.fetch_legacy_comments ~file_contents:s)
      in
      let comments = Dune_lang.Cst.extract_comments csts in
      let sexps = List.filter_map csts ~f:Dune_lang.Cst.abstract in
      files := Path.Source.Map.set !files path (sexps, comments);
      List.iter sexps ~f:(function
        | Dune_lang.Ast.List
            ( _
            , [ Atom (_, A "include")
              ; (Atom (loc, A fn) | Quoted_string (loc, fn))
              ] ) ->
          let dir = Path.Source.parent_exn path in
          let included_file = Path.Source.relative dir fn in
          if not (Path.exists (Path.source included_file)) then
            User_error.raise ~loc
              [ Pp.textf "File %s doesn't exist."
                  (Path.Source.to_string_maybe_quoted included_file)
              ];
          iter included_file
        | _ -> ())
    )
  in
  iter path;
  !files


let rename_basename base =
  match String.drop_prefix base ~prefix:File_tree.Dune_file.jbuild_fname with
  | None -> base
  | Some suffix -> "dune" ^ suffix

let upgrade_stanza stanza =
  let open Dune_lang.Ast in
  let simplify_field = function
    | "action"
    | "generate_runner"
    | "lint"
    | "preprocess"
    | "self_build_stubs_archive" ->
      false
    | _ -> true
  in
  let is_rule_field = function
    | "targets"
    | "deps"
    | "action"
    | "locks"
    | "fallback"
    | "mode" ->
      true
    | _ -> false
  in
  let rec uses_first_dep_var = function
    | Atom _
    | Quoted_string _ ->
      false
    | List (_, l) -> List.exists l ~f:uses_first_dep_var
    | Template x ->
      List.exists x.parts ~f:(function
        | Dune_lang.Template.Var { name = "<"; _ } -> true
        | _ -> false)
  in
  let rec map_var ~f = function
    | (Atom _ | Quoted_string _) as x -> x
    | List (loc, l) -> List (loc, List.map l ~f:(map_var ~f))
    | Template x ->
      Template
        { x with
          parts =
            List.map x.parts ~f:(function
              | Dune_lang.Template.Var v -> f v
              | x -> x)
        }
  in
  let upgrade_string s ~loc ~quoted =
    Jbuild_support.String_with_vars.upgrade_to_dune s ~loc ~quoted
      ~allow_first_dep_var:true
    |> String_with_vars.make |> String_with_vars.encode
    |> Dune_lang.Ast.add_loc ~loc
  in
  let rec upgrade = function
    | Atom (loc, A s) -> (
      match s with
      | "files_recursively_in" ->
        Atom (loc, Dune_lang.Atom.of_string "source_tree")
      | _ -> upgrade_string s ~loc ~quoted:false )
    | Template _ as x -> x
    | Quoted_string (loc, s) -> upgrade_string s ~loc ~quoted:true
    | List (loc, l) ->
      let l =
        match l with
        | [ (Atom (loc, A "fallback") as x) ] ->
          [ Atom (loc, Dune_lang.Atom.of_string "mode"); x ]
        | [ (Atom (_, A "include") as x)
          ; (Atom (loc, A s) | Quoted_string (loc, s))
          ] ->
          let base = Filename.basename s in
          let is_basename = base = s in
          let new_base = rename_basename base in
          let s =
            if is_basename then
              new_base
            else
              Filename.concat (Filename.dirname s) new_base
          in
          [ x; Dune_lang.Ast.add_loc ~loc (Dune_lang.atom_or_quoted_string s) ]
        | [ Atom _; List (_, [ Atom (_, A ":include"); Atom _ ]) ] ->
          List.map l ~f:upgrade
        | (Atom (_, A ("preprocess" | "lint")) as field) :: rest ->
          upgrade field
          :: List.map rest ~f:(fun x ->
                 map_var (upgrade x) ~f:(fun (v : Dune_lang.Template.var) ->
                     Dune_lang.Template.Var
                       ( if v.name = "<" then
                         { v with name = "input-file" }
                       else
                         v )))
        | (Atom (_, A "per_module") as field) :: specs ->
          upgrade field
          :: List.map specs ~f:(function
               | List (loc, [ spec; List (_, modules) ]) ->
                 List (loc, upgrade spec :: List.map modules ~f:upgrade)
               | sexp -> upgrade sexp)
        | [ (Atom (_, A "pps") as field); List (_, pps) ] -> (
          let pps, args =
            List.partition_map pps ~f:(function
              | (Atom (_, A s) | Quoted_string (_, s)) as sexp
                when String.is_prefix s ~prefix:"-" ->
                Right [ sexp ]
              | List (_, l) -> Right l
              | sexp -> Left sexp)
          in
          let args = List.concat args in
          (upgrade field :: pps)
          @
          match args with
          | [] -> []
          | _ -> Atom (loc, Dune_lang.Atom.of_string "--") :: args )
        | [ (Atom (_, A field_name) as field); List (_, args) ]
          when match (field_name, args) with
               | "rule", Atom (_, A field_name) :: _ -> is_rule_field field_name
               | _ -> simplify_field field_name ->
          upgrade field :: List.map args ~f:upgrade
        | _ -> List.map l ~f:upgrade
      in
      let l =
        if List.exists l ~f:uses_first_dep_var then
          List.map l ~f:(function
            | List (loc, (Atom (_, A "deps") as field) :: first :: rest) ->
              List
                ( loc
                , field
                  :: (let loc = Dune_lang.Ast.loc first in
                      List
                        ( loc
                        , [ Atom (loc, Dune_lang.Atom.of_string ":<"); first ]
                        ))
                  :: rest )
            | x -> x)
        else
          l
      in
      List (loc, l)
  in
  upgrade stanza

let load_jbuild_ignore path =
  let path = Path.source path in
  String.Set.of_list (Io.lines_of_file path)

let upgrade_file todo file sexps comments ~look_for_jbuild_ignore =
  let dir = Path.Source.parent_exn file in
  let new_file =
    let base = Path.Source.basename file in
    let new_base = rename_basename base in
    Path.Source.relative dir new_base
  in
  let sexps =
    List.filter sexps ~f:(function
      | Dune_lang.Ast.List (_, [ Atom (_, A "jbuild_version"); _ ]) -> false
      | _ -> true)
  in
  let sexps = List.map sexps ~f:upgrade_stanza in
  let sexps, extra_files_to_delete =
    (* Port the jbuild-ignore file if necessary *)
    let jbuild_ignore = Path.Source.relative dir "jbuild-ignore" in
    if not (look_for_jbuild_ignore && Path.exists (Path.source jbuild_ignore))
    then
      (sexps, [])
    else
      let data_only_dirs = load_jbuild_ignore jbuild_ignore in
      let stanza =
        Dune_lang.Ast.add_loc ~loc:Loc.none
          (List
             ( Dune_lang.atom "data_only_dirs"
             :: List.map
                  (String.Set.to_list data_only_dirs)
                  ~f:Dune_lang.atom_or_quoted_string ))
      in
      let sexps = stanza :: sexps in
      (sexps, [ jbuild_ignore ])
  in
  let sexps =
    Dune_lang.Parser.insert_comments
      (List.map ~f:Dune_lang.Cst.concrete sexps)
      comments
  in
  let contents = Format.asprintf "%a@?" Format_dune_lang.pp_top_sexps sexps in
  todo.to_rename_and_edit <-
    { original_file = file; new_file; extra_files_to_delete; contents }
    :: todo.to_rename_and_edit

(* This was obtained by trial and error. We should improve the opam parsing API
   to return better locations. *)
let rec end_offset_of_opam_value : OpamParserTypes.value -> int = function
  | Bool ((_, _, ofs), b) -> ofs + String.length (string_of_bool b)
  | Int ((_, _, ofs), x) -> ofs + String.length (string_of_int x)
  | String ((_, _, ofs), _) -> ofs + 1
  | Relop (_, _, _, v)
  | Prefix_relop (_, _, v)
  | Logop (_, _, _, v)
  | Pfxop (_, _, v) ->
    end_offset_of_opam_value v
  | Ident ((_, _, ofs), x) -> ofs + String.length x
  | List ((_, _, ofs), _)
  | Group ((_, _, ofs), _)
  | Option ((_, _, ofs), _, _) ->
    ofs (* this is definitely wrong *)
  | Env_binding ((_, _, ofs), _, _, _) -> ofs

(* probably wrong *)

let upgrade_opam_file todo fn =
  let open OpamParserTypes in
  let s = Io.read_file (Path.source fn) ~binary:true in
  let lb = Lexbuf.from_string s ~fname:(Path.Source.to_string fn) in
  let t =
    Opam_file.parse lb |> Opam_file.absolutify_positions ~file_contents:s
  in
  let substs = ref [] in
  let add_subst start stop repl = substs := (start, stop, repl) :: !substs in
  let replace_string (_, _, ofs) old repl =
    let len = String.length old in
    add_subst (ofs - len) ofs repl
  in
  let replace_jbuilder pos = replace_string pos "jbuilder" "dune" in
  let rec scan = function
    | String (jpos, "jbuilder") -> replace_jbuilder jpos
    | Option (pos, String (jpos, "jbuilder"), l) ->
      replace_jbuilder jpos;
      let _, _, start = pos in
      let stop = end_offset_of_opam_value (List.last l |> Option.value_exn) in
      add_subst (start + 1) stop
        (sprintf "build & >= %S"
           (Dune_lang.Syntax.Version.to_string
              !Dune_project.default_dune_language_version))
    | List
        (_, (String (jpos, "jbuilder") :: String (arg_pos, "subst") :: _ as l))
      ->
      replace_jbuilder jpos;
      let _, _, start = arg_pos in
      let stop = end_offset_of_opam_value (List.last l |> Option.value_exn) in
      let start = start + 1 in
      if start < stop then add_subst start stop ""
    | List
        ( _
        , ( String (jpos, "jbuilder")
            :: String (arg_pos, ("build" | "runtest")) :: _ as l ) ) ->
      replace_jbuilder jpos;
      let _, _, start = arg_pos in
      let stop = end_offset_of_opam_value (List.last l |> Option.value_exn) in
      let start = start + 1 in
      let stop =
        if start < stop then
          stop
        else
          start
      in
      add_subst start stop {| "-p" name "-j" jobs|}
    | Bool _
    | Int _
    | String _
    | Relop _
    | Logop _
    | Pfxop _
    | Ident _
    | Prefix_relop _ ->
      ()
    | List (_, l)
    | Group (_, l) ->
      List.iter l ~f:scan
    | Option (_, v, l) ->
      scan v;
      List.iter l ~f:scan
    | Env_binding (_, v1, _, v2) ->
      scan v1;
      scan v2
  in
  let rec scan_item = function
    | Section (_, s) -> List.iter s.section_items ~f:scan_item
    | Variable (_, _, v) -> scan v
  in
  List.iter t.file_contents ~f:scan_item;
  let substs = List.sort !substs ~compare:Poly.compare in
  if List.is_non_empty substs then (
    let buf = Buffer.create (String.length s + 128) in
    let ofs =
      List.fold_left substs ~init:0 ~f:(fun ofs (start, stop, repl) ->
          if not (ofs <= start && start <= stop) then
            Code_error.raise "Invalid text subsitution"
              [ ("ofs", Dyn.Encoder.int ofs)
              ; ("start", Dyn.Encoder.int start)
              ; ("stop", Dyn.Encoder.int stop)
              ; ("repl", Dyn.Encoder.string repl)
              ];
          Buffer.add_substring buf s ofs (start - ofs);
          Buffer.add_string buf repl;
          stop)
    in
    Buffer.add_substring buf s ofs (String.length s - ofs);
    let s' = Buffer.contents buf in
    if s <> s' then todo.to_edit <- (fn, s') :: todo.to_edit
  )

let upgrade_to_v1 todo dir =
  Dune_project.default_dune_language_version := (1, 0);
  let project = File_tree.Dir.project dir in
  let project_root = Dune_project.root project in
  ( if project_root = File_tree.Dir.path dir then
    let (_ : Dune_project.created_or_already_exist) =
      Dune_project.ensure_project_file_exists project
    in
    Package.Name.Map.iter (Dune_project.packages project) ~f:(fun pkg ->
        let fn = Package.opam_file pkg in
        if Path.exists (Path.source fn) then upgrade_opam_file todo fn) );
  if String.Set.mem (File_tree.Dir.files dir) File_tree.Dune_file.jbuild_fname
  then
    let fn =
      Path.Source.relative (File_tree.Dir.path dir)
        File_tree.Dune_file.jbuild_fname
    in
    if Io.with_lexbuf_from_file (Path.source fn) ~f:Dune_lexer.is_script then
      User_warning.emit
        ~loc:(Loc.in_file (Path.source fn))
        [ Pp.text
            "Cannot upgrade this jbuild file as it is using the OCaml syntax."
        ; Pp.text "You need to upgrade it manually."
        ]
    else
      let files = scan_included_files fn in
      Path.Source.Map.iteri files ~f:(fun fn' (sexps, comments) ->
          upgrade_file todo fn' sexps comments
            ~look_for_jbuild_ignore:(Path.Source.equal fn fn'))


let upgrade_to_v2 todo dir =
  let open Upgrader_v2 in
  Dune_project.default_dune_language_version := (2, 0);
  let project = File_tree.Dir.project dir in
  let _project_root = Dune_project.root project in
  update_project_file todo project

type project_version =
  | Jbuild_project
  | Dune2_project
  | Dune1_project

let fold_on_project_roots ~f ~init =
  File_tree.fold_with_progress
    ~traverse:Sub_dirs.Status.Set.normal_only
    ~init
    ~f

let detect_project_version project dir =
  (* TODO is it useful or can we use dune_version directly *)
  if String.Set.mem (File_tree.Dir.files dir) File_tree.Dune_file.jbuild_fname
  then
    Jbuild_project
  else
    let project_dune_version = Dune_project.dune_version project in
    let open Dune_lang.Syntax.Version.Infix in
    if project_dune_version >= (2, 0) then
      Dune2_project
    else
      Dune1_project

let detect_and_add_project_version dir acc =
  let project = File_tree.Dir.project dir in
  let detected_version = detect_project_version project dir in
  (dir, detected_version) :: acc

let upgrade () =
  let todo = { to_rename_and_edit = []; to_edit = [] } in
  let current_versions =
    fold_on_project_roots ~init:[] ~f:detect_and_add_project_version
  in
  List.iter current_versions ~f:(fun (dir, version) ->
      match version with
      | Jbuild_project ->
        Console.print
          [ Pp.textf "TODO: upgrade v0 -> v1: %s\n"
          (Path.Source.to_string_maybe_quoted (File_tree.Dir.path dir))];
          upgrade_to_v1 todo dir
      | Dune1_project ->
        Console.print
          [ Pp.textf "TODO: upgrade v1 -> v2: %s\n"
          (Path.Source.to_string_maybe_quoted (File_tree.Dir.path dir))];
          upgrade_to_v2 todo dir
      | Dune2_project ->
        Console.print
          [ Pp.textf "TODO: already upgraded at v2: %s\n"
          (Path.Source.to_string_maybe_quoted (File_tree.Dir.path dir))]);
  List.iter todo.to_edit ~f:(fun (fn, s) ->
      Console.print
        [ Pp.textf "Upgrading %s..." (Path.Source.to_string_maybe_quoted fn) ];
      Io.write_file (Path.source fn) s ~binary:true);
  List.iter todo.to_rename_and_edit ~f:(fun x ->
      let { original_file; new_file; extra_files_to_delete; contents } = x in
      Console.print
        [ Pp.textf "Upgrading %s to %s..."
            ( List.map
                (extra_files_to_delete @ [ original_file ])
                ~f:Path.Source.to_string_maybe_quoted
            |> String.enumerate_and )
            (Path.Source.to_string_maybe_quoted new_file)
        ];
      List.iter (original_file :: extra_files_to_delete) ~f:(fun p ->
          Path.unlink (Path.source p));
      Io.write_file (Path.source new_file) contents ~binary:true)
