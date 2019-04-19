open! Stdune
open Import
open Fiber.O

(* Return a mapping [Path.t -> Dune_lang.Ast.t list] containing [path]
   and all the files in includes, recursiverly *)
let scan_included_files path =
  let files = ref Path.Map.empty in
  let rec iter path =
    if not (Path.Map.mem !files path) then begin
      let s = Io.read_file path in
      let csts =
        Dune_lang.parse_cst_string s ~fname:(Path.to_string path)
          ~lexer:Dune_lang.Lexer.jbuild_token
        |> List.map ~f:(Dune_lang.Cst.fetch_legacy_comments
                          ~file_contents:s)
      in
      let comments = Dune_lang.Cst.extract_comments csts in
      let sexps = List.filter_map csts ~f:Dune_lang.Cst.abstract in
      files := Path.Map.add !files path (sexps, comments);
      List.iter sexps ~f:(function
        | Dune_lang.Ast.List
            (_,
             [ Atom (_, A "include")
             ; (Atom (loc, A fn) | Quoted_string (loc, fn))
             ]) ->
          let dir = Path.parent_exn path in
          let included_file = Path.relative dir fn in
          if not (Path.exists included_file) then
            Errors.fail loc "File %s doesn't exist."
              (Path.to_string_maybe_quoted included_file);
          iter included_file
        | _ -> ())
    end
  in
  iter path;
  !files

type rename_and_edit =
  { original_file : Path.t
  ; extra_files_to_delete : Path.t list
  ; new_file : Path.t
  ; contents : string
  }

type todo =
  { mutable to_rename_and_edit : rename_and_edit list
  ; mutable to_add : Path.Source.t list
  ; mutable to_edit : (Path.Source.t * string) list
  }

let rename_basename base =
  match String.drop_prefix base ~prefix:"jbuild" with
  | None -> base
  | Some suffix -> "dune" ^ suffix

let upgrade_stanza stanza =
  let open Dune_lang.Ast in
  let simplify_field = function
    | "action"
    | "generate_runner"
    | "lint"
    | "preprocess"
    | "self_build_stubs_archive"
      -> false
    | _ -> true
  in
  let is_rule_field = function
    | "targets" | "deps" | "action" | "locks" | "fallback" | "mode" -> true
    | _ -> false
  in
  let rec uses_first_dep_var = function
    | Atom _ | Quoted_string _ -> false
    | List (_, l) -> List.exists l ~f:uses_first_dep_var
    | Template x ->
      List.exists x.parts ~f:(function
        | Dune_lang.Template.Var { name = "<"; _ } -> true
        | _ -> false)
  in
  let rec map_var ~f = function
    | Atom _ | Quoted_string _ as x -> x
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
  let upgrade_string sexp =
    let loc = Dune_lang.Ast.loc sexp in
    Dune_lang.Decoder.parse String_with_vars.decode
      (Univ_map.singleton (Syntax.key Stanza.syntax) (0, 0))
      sexp
    |> String_with_vars.upgrade_to_dune ~allow_first_dep_var:true
    |> String_with_vars.encode
    |> Dune_lang.add_loc ~loc
  in
  let rec upgrade = function
    | Atom (loc, A s) as x ->
      begin match s with
      | "files_recursively_in" ->
        Atom (loc, Dune_lang.Atom.of_string "source_tree")
      | _ -> upgrade_string x
      end
    | Template _ as x -> x
    | Quoted_string _ as x -> upgrade_string x
    | List (loc, l) ->
      let l =
        match l with
        | [Atom (loc, A "fallback") as x] ->
          [Atom (loc, Dune_lang.Atom.of_string "mode"); x]
        | [ Atom (_, A "include") as x
          ; (Atom (loc, A s) | Quoted_string (loc, s)) ] ->
          let base = Filename.basename s in
          let is_basename = base = s in
          let new_base = rename_basename base in
          let s =
            if is_basename then
              new_base
            else
              Filename.concat (Filename.dirname s) new_base
          in
          [x;
           Dune_lang.add_loc ~loc
             (Dune_lang.atom_or_quoted_string s)]
        | [Atom _; List (_, [Atom (_, A ":include"); Atom _])] ->
          List.map l ~f:upgrade
        | Atom (_, A ("preprocess" | "lint")) as field :: rest ->
          upgrade field ::
          List.map rest ~f:(fun x ->
            map_var (upgrade x) ~f:(fun (v : Dune_lang.Template.var) ->
              Dune_lang.Template.Var
                (if v.name = "<" then
                   { v with name = "input-file" }
                 else
                   v)))
        | Atom (_, A "per_module") as field :: specs ->
          upgrade field ::
          List.map specs ~f:(function
            | List (loc, [spec; List (_, modules)]) ->
              List (loc, upgrade spec :: List.map modules ~f:upgrade)
            | sexp -> upgrade sexp)
        | [Atom (_, A "pps") as field; List (_, pps)] ->
          let pps, args =
            List.partition_map pps ~f:(function
              | Atom (_, A s) | Quoted_string (_, s) as sexp when
                  String.is_prefix s ~prefix:"-" ->
                Right [sexp]
              | List (_, l) ->
                Right l
              | sexp -> Left sexp)
          in
          let args = List.concat args in
          upgrade field
          :: pps
          @ (match args with
            | [] -> []
            | _ -> Atom (loc, Dune_lang.Atom.of_string "--") :: args)
        | [Atom (_, A field_name) as field; List (_, args)]
          when
            (match field_name, args with
             | "rule", Atom (_, A field_name) :: _ -> is_rule_field field_name
             | _ -> simplify_field field_name) ->
          upgrade field :: List.map args ~f:upgrade
        | _ ->
          List.map l ~f:upgrade
      in
      let l =
        if List.exists l ~f:uses_first_dep_var then
          List.map l ~f:(function
            | List (loc, (Atom (_, A "deps") as field :: first :: rest)) ->
              List (loc,
                    field ::
                    (let loc = Dune_lang.Ast.loc first in
                     List (loc,
                           [Atom (loc, Dune_lang.Atom.of_string ":<");
                            first])) ::
                    rest)
            | x -> x)
        else
          l
      in
      List (loc, l)
  in
  upgrade stanza

let upgrade_file todo file sexps comments ~look_for_jbuild_ignore =
  let dir = Path.parent_exn file in
  let new_file =
    let base = Path.basename file in
    let new_base = rename_basename base in
    Path.relative dir new_base
  in
  let sexps =
    List.filter sexps ~f:(function
      | Dune_lang.Ast.List (_, [Atom (_, A "jbuild_version"); _]) -> false
      | _ -> true)
  in
  let sexps = List.map sexps ~f:upgrade_stanza in
  let sexps, extra_files_to_delete =
    (* Port the jbuild-ignore file if necessary *)
    let jbuild_ignore = Path.relative dir "jbuild-ignore" in
    if not (look_for_jbuild_ignore && Path.exists jbuild_ignore) then
      (sexps, [])
    else begin
      let data_only_dirs = File_tree.load_jbuild_ignore jbuild_ignore in
      let stanza =
        Dune_lang.add_loc ~loc:Loc.none
          (List (Dune_lang.atom "data_only_dirs"
                 :: List.map (String.Set.to_list data_only_dirs)
                      ~f:Dune_lang.atom_or_quoted_string))
      in
      let sexps = stanza :: sexps in
      (sexps, [jbuild_ignore])
    end
  in
  let sexps =
    Dune_lang.insert_comments
      (List.map ~f:Dune_lang.Cst.concrete sexps)
      comments
  in
  let contents = Format.asprintf "%a@?" Format_dune_lang.pp_top_sexps sexps in
  todo.to_rename_and_edit <-
    { original_file = file
    ; new_file
    ; extra_files_to_delete
    ; contents
    } :: todo.to_rename_and_edit

(* This was obtained by trial and error. We should improve the opam
   parsing API to return better locations. *)
let rec end_offset_of_opam_value : OpamParserTypes.value -> int =
  function
  | Bool ((_, _, ofs), b) -> ofs + String.length (string_of_bool b)
  | Int ((_, _, ofs), x) -> ofs + String.length (string_of_int x)
  | String ((_, _, ofs), _) -> ofs + 1
  | Relop (_, _, _, v)
  | Prefix_relop (_, _, v)
  | Logop (_, _, _, v)
  | Pfxop (_, _, v) -> end_offset_of_opam_value v
  | Ident ((_, _, ofs), x) -> ofs + String.length x
  | List ((_, _, ofs), _)
  | Group ((_, _, ofs), _)
  | Option ((_, _, ofs), _, _) -> ofs (* this is definitely wrong *)
  | Env_binding ((_, _, ofs), _, _, _) -> ofs (* probably wrong *)

let upgrade_opam_file todo fn =
  let open OpamParserTypes in
  let s = Io.read_file (Path.source fn) ~binary:true in
  let lb = Lexing.from_string s in
  lb.lex_curr_p <-
    { pos_fname = Path.Source.to_string fn
    ; pos_lnum  = 1
    ; pos_bol   = 0
    ; pos_cnum  = 0
    };
  let t =
    Opam_file.parse lb
    |> Opam_file.absolutify_positions ~file_contents:s
  in
  let substs = ref [] in
  let add_subst start stop repl =
    substs := (start, stop, repl) :: !substs
  in
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
           (Syntax.Version.to_string
              !Dune_project.default_dune_language_version))
    | List (_, (String (jpos, "jbuilder")
                :: String (arg_pos, "subst") :: _ as l)) ->
      replace_jbuilder jpos;
      let _, _, start = arg_pos in
      let stop = end_offset_of_opam_value (List.last l |> Option.value_exn) in
      let start = start + 1 in
      if start < stop then add_subst start stop ""
    | List (_, (String (jpos, "jbuilder")
                :: String (arg_pos, ("build" | "runtest"))
                :: _ as l)) ->
      replace_jbuilder jpos;
      let _, _, start = arg_pos in
      let stop = end_offset_of_opam_value (List.last l |> Option.value_exn) in
      let start = start + 1 in
      let stop = if start < stop then stop else start in
      add_subst start stop {| "-p" name "-j" jobs|}
    | Bool _ | Int _ | String _ | Relop _ | Logop _ | Pfxop _
    | Ident _ | Prefix_relop _ -> ()
    | List (_, l) | Group (_, l) ->
      List.iter l ~f:scan
    | Option (_, v, l) ->
      scan v;
      List.iter l ~f:scan
    | Env_binding (_, v1, _, v2) ->
      scan v1;
      scan v2
  in
  let rec scan_item = function
    | Section (_, s) ->
      List.iter s.section_items ~f:scan_item
    | Variable (_, _, v) -> scan v
  in
  List.iter t.file_contents ~f:scan_item;
  let substs = List.sort !substs ~compare in
  if not (List.is_empty substs) then begin
    let buf = Buffer.create (String.length s + 128) in
    let ofs =
      List.fold_left substs ~init:0 ~f:(fun ofs (start, stop, repl) ->
        if not (ofs <= start && start <= stop) then
          Exn.code_error "Invalid text subsitution"
            [ "ofs", Sexp.Encoder.int ofs
            ; "start", Sexp.Encoder.int start
            ; "stop", Sexp.Encoder.int stop
            ; "repl", Sexp.Encoder.string repl
            ];
        Buffer.add_substring buf s ofs (start - ofs);
        Buffer.add_string buf repl;
        stop)
    in
    Buffer.add_substring buf s ofs (String.length s - ofs);
    let s' = Buffer.contents buf in
    if s <> s' then
      todo.to_edit <- (fn, s') :: todo.to_edit
  end

let upgrade_dir todo dir =
  let project = File_tree.Dir.project dir in
  let project_root = Dune_project.root project in
  if project_root = File_tree.Dir.path dir then begin
    (match Dune_project.ensure_project_file_exists project with
     | Already_exist -> ()
     | Created ->
       todo.to_add <- Dune_project.file project :: todo.to_add);
    Package.Name.Map.iter (Dune_project.packages project) ~f:(fun pkg ->
      let fn = Package.opam_file pkg in
      if Path.exists (Path.source fn) then upgrade_opam_file todo fn)
  end;
  Option.iter (File_tree.Dir.dune_file dir) ~f:(fun dune_file ->
    match dune_file.kind, dune_file.contents with
    | Dune, _ -> ()
    | Jbuild, Ocaml_script fn ->
      Errors.warn (Loc.in_file (Path.source fn))
        "Cannot upgrade this jbuild file as it is using the OCaml syntax.\n\
         You need to upgrade it manually."
    | Jbuild, Plain { path; sexps = _ } ->
      let files = scan_included_files (Path.source path) in
      Path.Map.iteri files ~f:(fun fn (sexps, comments) ->
        upgrade_file todo fn sexps comments
          ~look_for_jbuild_ignore:(fn = Path.source path)))

let upgrade ft =
  Dune_project.default_dune_language_version := (1, 0);
  let todo =
    { to_rename_and_edit = []
    ; to_add = []
    ; to_edit = []
    }
  in
  File_tree.fold ft ~traverse_ignored_dirs:false ~init:() ~f:(fun dir () ->
    upgrade_dir todo dir);
  let git = lazy (
    match Bin.which ~path:(Env.path Env.initial) "git" with
    | Some x -> x
    | None -> Utils.program_not_found "git" ~loc:None)
  in
  let has_git_table = Hashtbl.create 128 in
  let rec has_git path =
    match Hashtbl.find has_git_table path with
    | None ->
      let v =
        if Path.is_directory (Path.relative path ".git") then
          Some path
        else
          match Path.parent path with
          | None -> None
          | Some p -> has_git p
      in
      Hashtbl.add has_git_table path v;
      v
    | Some v -> v
  in
  let log fmt =
    Printf.ksprintf print_to_console fmt
  in
  let* () =
    Fiber.map_all_unit todo.to_add ~f:(fun fn ->
      match has_git (Path.source (Path.Source.parent_exn fn)) with
      | Some dir ->
        Process.run Strict ~dir ~env:Env.initial
          (Lazy.force git)
          ["add"; Path.reach (Path.source fn) ~from:dir]
      | None -> Fiber.return ())
  in
  List.iter todo.to_edit ~f:(fun (fn, s) ->
    log "Upgrading %s...\n" (Path.Source.to_string_maybe_quoted fn);
    Io.write_file (Path.source fn) s ~binary:true);
  Fiber.map_all_unit todo.to_rename_and_edit ~f:(fun x ->
    let { original_file
        ; new_file
        ; extra_files_to_delete
        ; contents
        } = x
    in
    log "Upgrading %s to %s...\n"
      (List.map (extra_files_to_delete @ [original_file])
         ~f:Path.to_string_maybe_quoted |> String.enumerate_and)
      (Path.to_string_maybe_quoted new_file);
    (match has_git (Path.parent_exn original_file) with
     | Some dir ->
       Fiber.map_all_unit extra_files_to_delete ~f:(fun fn ->
         Process.run Strict ~dir ~env:Env.initial
           (Lazy.force git)
           ["rm"; Path.reach fn ~from:dir])
       >>>
       Process.run Strict ~dir ~env:Env.initial
         (Lazy.force git)
         [ "mv"
         ; Path.reach original_file ~from:dir
         ; Path.reach new_file ~from:dir
         ]
     | None ->
       List.iter (original_file :: extra_files_to_delete) ~f:Path.unlink;
       Fiber.return ())
    >>| fun () ->
    Io.write_file new_file contents ~binary:true)
