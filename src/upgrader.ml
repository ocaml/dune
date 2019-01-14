open! Stdune
open Import
open Fiber.O

(* Return a mapping [Path.t -> Dune_lang.Ast.t list] containing [path]
   and all the files in includes, recursiverly *)
let scan_included_files path =
  let files = ref Path.Map.empty in
  let rec iter path =
    if not (Path.Map.mem !files path) then begin
      let sexps =
        Dune_lang.Io.load path
          ~lexer:Dune_lang.Lexer.jbuild_token
          ~mode:Many
      in
      files := Path.Map.add !files path sexps;
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
  ; mutable to_add : Path.t list
  }

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
        | [Atom _; List (_, [Atom (_, A ":include"); Atom _])] ->
          List.map l ~f:upgrade
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

let upgrade_file todo file sexps ~look_for_jbuild_ignore =
  let dir = Path.parent_exn file in
  let new_file =
    let base = Path.basename file in
    let new_base =
      match String.drop_prefix base ~prefix:"jbuild" with
      | None -> base
      | Some suffix -> "dune" ^ suffix
    in
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
  let contents = Format.asprintf "%a@?" Dune_fmt.pp_top_sexps sexps in
  todo.to_rename_and_edit <-
    { original_file = file
    ; new_file
    ; extra_files_to_delete
    ; contents
    } :: todo.to_rename_and_edit

let upgrade_dir todo dir =
  let project = File_tree.Dir.project dir in
  (match Dune_project.ensure_project_file_exists project with
   | Already_exist -> ()
   | Created ->
     todo.to_add <- Dune_project.file project :: todo.to_add);
  Option.iter (File_tree.Dir.dune_file dir) ~f:(fun dune_file ->
    match dune_file.kind, dune_file.contents with
    | Dune, _ -> ()
    | Jbuild, Ocaml_script fn ->
      Errors.warn (Loc.in_file fn)
        "Cannot upgrade this jbuild file as it is using the OCaml syntax.\n\
         You need to upgrade it manually."
    | Jbuild, Plain { path; sexps = _ } ->
      let files = scan_included_files path in
      Path.Map.iteri files ~f:(fun fn sexps ->
        upgrade_file todo fn sexps ~look_for_jbuild_ignore:(fn = path)))

let upgrade ft =
  let todo =
    { to_rename_and_edit = []
    ; to_add = []
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
  Fiber.map_all_unit todo.to_add ~f:(fun fn ->
    match has_git (Path.parent_exn fn) with
    | Some dir ->
      Process.run Strict ~dir ~env:Env.initial
        (Lazy.force git)
        ["add"; Path.reach fn ~from:dir]
    | None -> Fiber.return ())
  >>= fun () ->
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
