open Import
open Memo.O

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

  let dependencies t =
    let+ platform = Lock_dir.Sys_vars.solver_env in
    Dune_pkg.Lock_dir.Conditional_choice.choose_for_platform t.lock_pkg.depends ~platform
    |> Option.value ~default:[]
    |> Package.Name.Set.of_list_map ~f:(fun (dep : Dune_pkg.Lock_dir.Dependency.t) ->
      dep.name)
  ;;

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

module Mounted = struct
  type loaded =
    { tree : Source_tree.Rules.Mounted.t
    ; projects : (Dune_project.t * Source_tree.Rules.Dir.t) list
    }

  type t =
    { candidate : Candidate.t
    ; loaded : (loaded option, User_message.t list) result
    }

  let candidate t = t.candidate

  let kind t =
    Result.map t.loaded ~f:(function
      | None -> `Opam
      | Some _ -> `Dune)
  ;;

  let kind_exn t =
    match kind t with
    | Ok kind -> Memo.return kind
    | Error messages ->
      List.map messages ~f:(fun message ->
        Exn_with_backtrace.capture (User_error.E message))
      |> Fiber.reraise_all
      |> Memo.of_reproducible_fiber
  ;;

  let tree t =
    match t.loaded with
    | Ok (Some loaded) -> Some loaded.tree
    | Ok None | Error _ -> None
  ;;

  let projects t =
    match t.loaded with
    | Ok (Some loaded) -> loaded.projects
    | Ok None | Error _ -> []
  ;;
end

module Mounted_source_tree = Source_tree.Rules.Mounted
module Source_substs = Dune_pkg.Substs.Make (Memo)
module Package_action = Dune_lang.Action
module Variable = Opam_build.Variable

type source_transformation =
  | Substitute of String_with_vars.t * String_with_vars.t
  | Patch of String_with_vars.t

type 'a transformation_result =
  | Supported of 'a
  | Unsupported

type variable_resolution =
  | Resolved of OpamVariable.variable_contents option
  | Requires_package_build

let resolve_package_variable
      candidates
      candidate
      { Dune_pkg.Package_variable.scope; name }
  =
  let package =
    match scope with
    | Self -> Candidate.name candidate
    | Package package -> package
  in
  match
    List.find candidates ~f:(fun candidate ->
      Package.Name.equal (Candidate.name candidate) package)
  with
  | None ->
    let value =
      match Package_variable_name.to_string name with
      | "build-id" -> None
      | _ -> Variable.package_default ~present:false name
    in
    Resolved value
  | Some package ->
    let variables =
      Dune_pkg.Lock_dir.Pkg_info.variables (Candidate.lock_pkg package).info
    in
    (match
       Option.first_some
         (Package_variable_name.Map.find variables name)
         (Variable.package_default ~present:true name)
     with
     | Some value -> Resolved (Some value)
     | None -> Requires_package_build)
;;

let rec action_contains_source_transformation = function
  | Package_action.Patch _ | Substitute _ -> true
  | Progn actions | Concurrent actions | Pipe (_, actions) ->
    List.exists actions ~f:action_contains_source_transformation
  | With_accepted_exit_codes (_, action)
  | Chdir (_, action)
  | Setenv (_, _, action)
  | Redirect_out (_, _, _, action)
  | Redirect_in (_, _, action)
  | Ignore (_, action)
  | No_infer action
  | Withenv (_, action)
  | When (_, action) -> action_contains_source_transformation action
  | _ -> false
;;

let rec flatten_progn = function
  | Package_action.Progn actions -> List.concat_map actions ~f:flatten_progn
  | Withenv (_, action) -> flatten_progn action
  | action -> [ action ]
;;

let resolve_global_variable solver_env name =
  let value =
    match
      Package_variable_name.to_string name |> Pform.Var.of_opam_global_variable_name
    with
    | Some (Pkg ((Switch | Jobs | User | Group) as variable)) ->
      Variable.context ~solver_env variable
    | Some (Pkg (Build | Prefix | Section_dir _)) -> None
    | _ ->
      Dune_pkg.Solver_env.get solver_env name
      |> Option.map ~f:Dune_pkg.Variable_value.to_opam_variable_contents
  in
  match value with
  | Some value -> Resolved (Some value)
  | None -> Requires_package_build
;;

let expand_condition_pform candidates candidate solver_env unsupported ~source = function
  | Pform.Var (Pkg variable) ->
    let values =
      match Variable.context ~solver_env variable with
      | Some value -> Variable.dune_value value
      | None ->
        unsupported := true;
        [ Value.String "" ]
    in
    Memo.return (Ok values)
  | Macro ({ macro = Pkg | Pkg_self; _ } as invocation) ->
    let loc = Dune_sexp.Template.Pform.loc source in
    let variable =
      match Dune_pkg.Package_variable.of_macro_invocation ~loc invocation with
      | Ok variable -> variable
      | Error `Unexpected_macro -> Code_error.raise "Unexpected package variable macro" []
    in
    (match resolve_package_variable candidates candidate variable with
     | Resolved (Some value) -> Variable.dune_value value |> Result.ok |> Memo.return
     | Resolved None -> Memo.return (Error (`Undefined_pkg_var variable.name))
     | Requires_package_build ->
       unsupported := true;
       Memo.return (Ok [ Value.String "" ]))
  | _ ->
    unsupported := true;
    Memo.return (Ok [ Value.String "" ])
;;

let eval_source_condition candidates candidate solver_env condition =
  let unsupported = ref false in
  let expand sw =
    String_expander.Memo.expand_result_deferred_concat
      sw
      ~mode:Many
      ~f:(expand_condition_pform candidates candidate solver_env unsupported)
  in
  let+ enabled =
    Slang_expand.eval_blang
      condition
      ~dir:(Path.build (Candidate.artifact_root candidate))
      ~f:expand
  in
  if !unsupported then None else Some enabled
;;

let source_transformations candidates candidate solver_env = function
  | None | Some Dune_pkg.Lock_dir.Build_command.Dune -> Memo.return (Supported [])
  | Some (Action action) ->
    let rec loop transformations = function
      | Package_action.Substitute (source, target) :: actions ->
        (match String_with_vars.text_only source, String_with_vars.text_only target with
         | Some _, Some _ -> loop (Substitute (source, target) :: transformations) actions
         | None, _ | _, None -> Memo.return Unsupported)
      | Patch path :: actions ->
        (match String_with_vars.text_only path with
         | Some _ -> loop (Patch path :: transformations) actions
         | None -> Memo.return Unsupported)
      | Package_action.When (condition, action) :: actions
        when action_contains_source_transformation action ->
        let condition = Slang.Blang.remove_locs condition |> Slang.simplify_blang in
        eval_source_condition candidates candidate solver_env condition
        >>= (function
         | None -> Memo.return Unsupported
         | Some true -> loop transformations (flatten_progn action @ actions)
         | Some false -> loop transformations actions)
      | action :: actions ->
        if
          action_contains_source_transformation action
          || List.exists actions ~f:action_contains_source_transformation
        then Memo.return Unsupported
        else Memo.return (Supported (List.rev transformations))
      | [] -> Memo.return (Supported (List.rev transformations))
    in
    loop [] (flatten_progn action)
;;

let local_path path =
  String_with_vars.text_only path
  |> Option.map ~f:(Path.Local.parse_string_exn ~loc:(String_with_vars.loc path))
;;

let logical_path candidate path =
  Path.Build.append_local (Candidate.artifact_root candidate) path
;;

let read_source_file candidate layers path =
  Mounted_source_tree.read_file ~logical:(logical_path candidate path) ~layers
;;

let merge_dependencies contents =
  List.concat_map contents ~f:(fun { Source_tree.Rules.File.dependencies; _ } ->
    dependencies)
  |> Path.Set.of_list
  |> Path.Set.to_list
;;

let apply_patch candidate layers path =
  let loc = String_with_vars.loc path in
  let path = Option.value_exn (local_path path) in
  let logical = logical_path candidate path in
  let* patch_contents = read_source_file candidate layers path in
  let patch_contents =
    match patch_contents with
    | Some contents -> contents
    | None ->
      User_error.raise
        ~loc
        [ Pp.textf "Patch file %S does not exist" (Path.Local.to_string path) ]
  in
  let patches =
    Dune_patch.File_patch.parse
      ~loc
      ~patch_file:(Path.build logical)
      patch_contents.contents
  in
  Memo.List.fold_left patches ~init:layers ~f:(fun layers patch ->
    let source = Dune_patch.File_patch.source patch in
    let target = Dune_patch.File_patch.target patch in
    let* source_contents =
      match source with
      | None -> Memo.return None
      | Some source -> read_source_file candidate layers source
    in
    let* target_contents =
      match target with
      | None -> Memo.return None
      | Some target -> read_source_file candidate layers target
    in
    let dependencies =
      merge_dependencies
        (patch_contents :: List.filter_opt [ source_contents; target_contents ])
    in
    let contents =
      Dune_patch.File_patch.apply
        patch
        (Option.map source_contents ~f:(fun { contents; _ } -> contents))
    in
    let executable =
      let contents =
        if Dune_patch.File_patch.preserves_source_mode patch
        then source_contents
        else target_contents
      in
      match contents with
      | None -> false
      | Some { executable; _ } -> executable
    in
    let layers =
      match source with
      | Some source when Dune_patch.File_patch.removes_source patch ->
        layers
        @ [ Mounted_source_tree.Layer.delete ~logical:(logical_path candidate source) ]
      | Some _ | None -> layers
    in
    let layers =
      match target, contents with
      | Some target, Some contents ->
        layers
        @ [ Mounted_source_tree.Layer.contents
              ~logical:(logical_path candidate target)
              ~contents
              ~executable
              ~dependencies
          ]
      | Some target, None ->
        layers
        @ [ Mounted_source_tree.Layer.delete ~logical:(logical_path candidate target) ]
      | None, None -> layers
      | None, Some _ ->
        Code_error.raise
          "Patch produced contents without a target"
          [ "patch", Dune_patch.File_patch.to_dyn patch ]
    in
    Memo.return layers)
;;

let substitution_env candidates candidate solver_env unsupported = function
  | Dune_pkg.Substs.Variable.Global name ->
    (match resolve_global_variable solver_env name with
     | Resolved value -> Memo.return value
     | Requires_package_build ->
       unsupported := true;
       Memo.return None)
  | Package variable ->
    (match resolve_package_variable candidates candidate variable with
     | Resolved value -> Memo.return value
     | Requires_package_build ->
       unsupported := true;
       Memo.return None)
;;

let apply_substitution candidates candidate solver_env layers source target =
  let loc = String_with_vars.loc source in
  let source = Option.value_exn (local_path source) in
  let target = Option.value_exn (local_path target) in
  let* source_contents = read_source_file candidate layers source in
  let source_contents =
    match source_contents with
    | Some contents -> contents
    | None ->
      User_error.raise
        ~loc
        [ Pp.textf "Substitution input %S does not exist" (Path.Local.to_string source) ]
  in
  let* target_contents = read_source_file candidate layers target in
  let executable =
    match target_contents with
    | None -> false
    | Some { executable; _ } -> executable
  in
  let dependencies =
    merge_dependencies (source_contents :: Option.to_list target_contents)
  in
  let unsupported = ref false in
  let+ contents =
    Source_substs.subst_contents
      (substitution_env candidates candidate solver_env unsupported)
      (Candidate.name candidate)
      ~src:(Path.build (logical_path candidate source))
      source_contents.contents
  in
  if !unsupported
  then Unsupported
  else
    Supported
      (layers
       @ [ Mounted_source_tree.Layer.contents
             ~logical:(logical_path candidate target)
             ~contents
             ~executable
             ~dependencies
         ])
;;

let apply_source_transformations candidates candidate solver_env layers transformations =
  let rec loop layers = function
    | [] -> Memo.return (Supported layers)
    | Substitute (source, target) :: transformations ->
      apply_substitution candidates candidate solver_env layers source target
      >>= (function
       | Unsupported -> Memo.return Unsupported
       | Supported layers -> loop layers transformations)
    | Patch path :: transformations ->
      let* layers = apply_patch candidate layers path in
      loop layers transformations
  in
  loop layers transformations
;;

module Scan_dune_files =
  Source_tree.Rules.Dir.Make_map_reduce
    (Memo)
    (Monoid.Product (Monoid.Exists) (Monoid.Exists))

let prepare candidates candidate =
  let* direct = Candidate.dependencies candidate in
  let candidates =
    List.filter candidates ~f:(fun other ->
      let name = Candidate.name other in
      Package.Name.equal name (Candidate.name candidate)
      || Package.Name.Set.mem direct name)
  in
  let opaque = None in
  match Candidate.source_root candidate with
  | None -> Memo.return opaque
  | Some backing_root ->
    let* platform = Lock_dir.Sys_vars.solver_env in
    let lock_pkg = Candidate.lock_pkg candidate in
    let build =
      Dune_pkg.Lock_dir.Conditional_choice.choose_for_platform
        lock_pkg.build_command
        ~platform
    in
    let* transformations = source_transformations candidates candidate platform build in
    (match transformations with
     | Unsupported -> Memo.return opaque
     | Supported transformations ->
       let* input = source candidate in
       let logical_root = Candidate.artifact_root candidate in
       let module Layer = Source_tree.Rules.Mounted.Layer in
       let layers =
         (if List.is_empty input.files
          then []
          else [ Layer.directory ~logical_root ~backing_root ])
         @ (Layer.directory ~logical_root ~backing_root:(Candidate.files_dir candidate)
            :: List.map lock_pkg.info.extra_sources ~f:(fun (local, source) ->
              Layer.file
                ~logical:(Path.Build.append_local logical_root local)
                ~backing:(Fetch_rules.target source `File)))
       in
       let* layers =
         apply_source_transformations candidates candidate platform layers transformations
       in
       (match layers with
        | Unsupported -> Memo.return opaque
        | Supported layers ->
          let* tree = Source_tree.Rules.Mounted.load ~logical_root ~layers in
          let projects = Source_tree.Rules.Mounted.projects tree in
          let name = Candidate.name candidate in
          let* has_dune_file, has_script =
            Scan_dune_files.map_reduce
              (Source_tree.Rules.Mounted.root tree)
              ~traverse:Source_dir_status.Set.all
              ~trace_event_name:"Package source classification"
              ~f:(fun dir ->
                match Source_tree.Rules.Dir.dune_file dir with
                | None -> Memo.return (false, false)
                | Some file ->
                  (match Source.Dune_file.kind file with
                   | Ocaml_script _ -> Memo.return (false, true)
                   | Plain ->
                     let project = Source_tree.Rules.Dir.project dir in
                     let+ enabled =
                       match
                         Package.Name.Map.find
                           (Dune_project.including_hidden_packages project)
                           name
                       with
                       | None -> Memo.return false
                       | Some package -> Package_enabled.eval package
                     in
                     enabled, false))
          in
          if has_script || not has_dune_file
          then Memo.return opaque
          else (
            let projects =
              List.map projects ~f:(fun (project, dir) ->
                ( Dune_project.set_package_version
                    project
                    ~package:name
                    ~version:lock_pkg.info.version
                , dir ))
            in
            Memo.return (Some { Mounted.tree; projects }))))
;;

let mounted_by_name =
  Per_context.create_by_name ~name:"mounted-packages" (fun context ->
    let+ candidates = candidates context in
    Package.Name.Map.of_list_map_exn candidates ~f:(fun candidate ->
      let loaded =
        Memo.lazy_ ~name:"prepare-mounted-package" (fun () ->
          let+ loaded = Memo.collect_user_errors (prepare candidates candidate) in
          { Mounted.candidate; loaded })
      in
      Candidate.name candidate, loaded))
  |> Staged.unstage
;;

let mounted context =
  let* mounted = mounted_by_name context in
  Package.Name.Map.values mounted |> Memo.parallel_map ~f:Memo.Lazy.force
;;

let find_mounted context name =
  let* mounted = mounted_by_name context in
  match Package.Name.Map.find mounted name with
  | None -> Memo.return None
  | Some mounted -> Memo.Lazy.force mounted >>| Option.some
;;
