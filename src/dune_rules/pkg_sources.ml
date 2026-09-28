open Import
open Memo.O
module Gen_rules = Build_config.Gen_rules

let root_dir_basename = ".lockfile"
let artifact_dir_basename = "pkg"

let package_of_artifact_path path =
  match Path.Build.extract_build_context path with
  | None -> None
  | Some (_, source) ->
    (match Path.Source.explode source |> Filename.L.to_string with
     | root :: packages :: name :: _
       when String.equal root root_dir_basename
            && String.equal packages artifact_dir_basename ->
       Package.Name.of_string_opt name
     | _ -> None)
;;

module Candidate = struct
  type t =
    { lock_pkg : Dune_pkg.Lock_dir.Pkg.t
    ; source_root : Path.Build.t option
    ; files_dir : Path.Build.t
    ; artifact_root : Path.Build.t
    }

  let name t = t.lock_pkg.info.name
  let source_root t = t.source_root
  let lock_pkg t = t.lock_pkg
  let files_dir t = t.files_dir
  let artifact_root t = t.artifact_root

  let make context (lock_pkg : Dune_pkg.Lock_dir.Pkg.t) ~files_dir =
    let source_root =
      Option.map lock_pkg.info.source ~f:(fun source ->
        Fetch_rules.target source `Directory)
    in
    let artifact_root =
      Path.Build.L.relative
        (Context_name.build_dir context)
        [ root_dir_basename
        ; artifact_dir_basename
        ; Package.Name.to_string lock_pkg.info.name
        ]
    in
    { lock_pkg; source_root; files_dir; artifact_root }
  ;;

  let paths t =
    let name = name t in
    let root =
      Path.Build.L.relative t.artifact_root [ ".opam"; Package.Name.to_string name ]
    in
    Opam_build.Paths.of_root name ~root ~relative:Path.Build.relative
  ;;
end

let candidates =
  Per_context.create_by_name ~name:"mounted-package-candidates" (fun context ->
    let* active = Lock_dir.lock_dir_active context in
    if not active
    then Memo.return []
    else
      let+ lock_dir = Lock_dir.get_exn context
      and+ lock_dir_path = Lock_dir.get_path context >>| Option.value_exn
      and+ platform = Lock_dir.Sys_vars.solver_env in
      Dune_pkg.Lock_dir.packages_on_platform lock_dir ~platform
      |> Package.Name.Map.values
      |> List.map ~f:(fun (lock_pkg : Dune_pkg.Lock_dir.Pkg.t) ->
        let version =
          Option.some_if
            (Dune_pkg.Lock_dir.uses_versioned_paths lock_dir)
            lock_pkg.info.version
        in
        let files_dir =
          Dune_pkg.Lock_dir.Pkg.files_dir
            lock_pkg.info.name
            version
            ~lock_dir:lock_dir_path
          |> Path.as_in_build_dir_exn
        in
        Candidate.make context lock_pkg ~files_dir))
  |> Staged.unstage
;;

let find_candidate context name =
  let+ candidates = candidates context in
  List.find candidates ~f:(fun candidate ->
    Package.Name.equal name (Candidate.name candidate))
;;

let source candidate =
  let rec loop root local =
    let dir = Path.Build.append_local root local in
    let* files, dirs = Build_system.directory_target_contents ~dir in
    let files =
      Filename.Array.Set.to_list files
      |> List.map ~f:(fun file ->
        ( Path.build (Path.Build.relative_fname dir file)
        , Path.Local.relative_fname local file ))
    in
    let+ subdirs =
      Filename.Array.Set.to_list dirs
      |> Memo.List.map ~f:(fun dir -> loop root (Path.Local.relative_fname local dir))
    in
    { Opam_build.Source_input.files =
        files
        @ List.concat_map subdirs ~f:(fun { Opam_build.Source_input.files; _ } -> files)
    ; directories =
        local
        :: List.concat_map subdirs ~f:(fun { Opam_build.Source_input.directories; _ } ->
          directories)
    }
  in
  let* empty_directories =
    match (Candidate.lock_pkg candidate).info.source with
    | None -> Memo.return None
    | Some source ->
      Lock_dir.source_kind source
      >>= (function
       | `Local (`File, _) | `Fetch -> Memo.return None
       | `Local (`Directory, dir) ->
         let+ files, dirs = Fetch_rules.local_source_contents ~loc:(fst source.url) dir in
         Option.some_if (List.is_empty files) dirs)
  in
  match empty_directories, Candidate.source_root candidate with
  | Some directories, _ ->
    (* Empty inputs cannot be represented by an engine directory target. *)
    Memo.return { Opam_build.Source_input.files = []; directories }
  | None, None -> Memo.return { Opam_build.Source_input.files = []; directories = [] }
  | None, Some root -> loop root Path.Local.root
;;

let source_copy_rule ~dir ~source_dir filename =
  let file = Source_tree.Rules.Dir.file source_dir filename in
  let* materialization = Source_tree.Rules.File.materialization file in
  let dst = Path.Build.relative_fname dir filename in
  match materialization with
  | None ->
    Code_error.raise
      "Mounted source file selected without materialization"
      [ "file", Source_tree.Rules.File.source_path file |> Source_path.to_dyn ]
  | Some (Copy src) ->
    let { Action_builder.With_targets.build; targets } = Action_builder.copy ~src ~dst in
    Rule.make ~info:(Rule.Info.Source_file_copy src) ~targets build |> Memo.return
  | Some (Write { contents; executable; dependencies }) ->
    let perm =
      if executable
      then Dune_lang.Action.File_perm.Executable
      else Dune_lang.Action.File_perm.Normal
    in
    let { Action_builder.With_targets.build; targets } =
      Action_builder.write_file ~perm dst contents
    in
    let build =
      let open Action_builder.O in
      let* () = Action_builder.paths dependencies in
      build
    in
    let source = Source_tree.Rules.File.path file in
    Rule.make ~info:(Rule.Info.Source_file_copy source) ~targets build |> Memo.return
;;

let add_artifact_source_rules ~dir ~source_dir rules =
  let add_sources (generated : Gen_rules.Rules.t) =
    let rules =
      let* generated_rules = generated.rules in
      let generated_here =
        Rules.find generated_rules (Path.build dir) |> Rules.Dir_rules.consume
      in
      let selected =
        Dune_engine.Source_selection.select
          ~dir
          ~source_dir:(Source_tree.Rules.Dir.path source_dir)
          ~build_dir_only_sub_dirs:
            (Path.Build.Map.find generated.build_dir_only_sub_dirs dir
             |> Option.value ~default:Subdir_set.empty)
          ~source_filenames:(Source_tree.Rules.Dir.filenames source_dir)
          ~source_dirs:(Source_tree.Rules.Dir.sub_dir_names source_dir)
          generated_here.rules
      in
      let selected_rules = Rule.Set.of_list selected.rules in
      let generated_rules =
        Rules.filter_rules generated_rules ~f:(fun rule ->
          (not (Path.Build.equal rule.targets.root dir))
          || Rule.Set.mem selected_rules rule)
        |> Rules.map_rules ~f:(fun (rule : Rule.t) ->
          match rule.mode with
          | Promote _ -> Rule.set_mode rule Standard
          | Standard | Fallback | Ignore_source_files -> rule)
      in
      let* source_rules =
        Filename.Array.Set.to_list selected.source_filenames
        |> Memo.List.map ~f:(fun filename -> source_copy_rule ~dir ~source_dir filename)
      in
      Memo.return (Rules.union (Rules.of_rules source_rules) generated_rules)
    in
    { generated with rules }
  in
  match rules with
  | Gen_rules.Unknown_context -> Gen_rules.Unknown_context
  | Rules generated -> Gen_rules.Rules (add_sources generated)
  | Redirect_to_parent generated -> Gen_rules.Redirect_to_parent (add_sources generated)
;;
