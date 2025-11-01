open Import
open Memo.O
module Gen_rules = Build_config.Gen_rules
module Solver_env = Dune_pkg.Solver_env
module Package_name = Dune_pkg.Package_name

let solver_env
      ~solver_env_from_current_system
      ~solver_env_from_context
      ~unset_solver_vars_from_context
  =
  let solver_env =
    [ solver_env_from_current_system; solver_env_from_context ]
    |> List.filter_opt
    |> List.fold_left ~init:Solver_env.with_defaults ~f:Solver_env.extend
  in
  match unset_solver_vars_from_context with
  | None -> solver_env
  | Some unset_solver_vars -> Solver_env.unset_multi solver_env unset_solver_vars
;;

module Spec = struct
  type ('path, 'target) t =
    { target : 'target
    ; lock_dir : string
    ; packages : Dune_pkg.Local_package.t Package.Name.Map.t
    ; repos : Dune_pkg.Opam_repo.t list
    ; env : Dune_pkg.Solver_env.t
    ; constraints : Package_dependency.t list
    ; selected_depopts : Dune_lang.Package_name.t list
    ; pins : Dune_pkg.Resolved_package.t Dune_lang.Package_name.Map.t
    ; version_preference : Dune_pkg.Version_preference.t
    }

  let name = "lock"
  let version = 1
  let bimap t _ g = { t with target = g t.target }
  let is_useful_to ~memoize = memoize
  let encode_opam_file v : Sexp.t = Atom (OpamFile.OPAM.write_to_string v)

  let encode_opam_package v : Sexp.t =
    List
      [ List [ Atom "name"; Atom (v |> OpamPackage.name |> OpamPackage.Name.to_string) ]
      ; List
          [ Atom "version"
          ; Atom (v |> OpamPackage.version |> OpamPackage.Version.to_string)
          ]
      ]
  ;;

  let encode_resolved_pkg v : Sexp.t =
    let module R = Dune_pkg.Resolved_package in
    List
      [ List [ Atom "opam_file"; v |> R.opam_file |> encode_opam_file ]
      ; List [ Atom "package"; v |> R.package |> encode_opam_package ]
      ; List [ Atom "loc"; Atom (v |> R.loc |> Loc.to_dyn |> Dyn.to_string) ]
      ; List [ Atom "dune_build"; Atom (v |> R.dune_build |> Bool.to_string) ]
      ]
  ;;

  let encode
        { target
        ; lock_dir
        ; packages
        ; repos
        ; env
        ; constraints
        ; selected_depopts
        ; pins
        ; version_preference
        }
        _encode_path
        encode_target
    : Sexp.t
    =
    let packages : Sexp.t =
      match Dune_pkg.Package_universe.dependency_digest packages with
      | None -> Atom "no packages"
      | Some hash ->
        List [ Atom "hash"; Atom (Dune_pkg.Local_package.Dependency_hash.to_string hash) ]
    in
    let repos : Sexp.t =
      List
        (List.map repos ~f:(fun repo ->
           let dyn = repo |> Dune_pkg.Opam_repo.to_dyn |> Dyn.to_string in
           let digest =
             repo |> Dune_pkg.Opam_repo.content_digest |> Dune_digest.to_string
           in
           Sexp.List
             [ Sexp.List [ Sexp.Atom "dyn"; Sexp.Atom dyn ]
             ; Sexp.List [ Sexp.Atom "digest"; Sexp.Atom digest ]
             ]))
    in
    let env : Sexp.t =
      env |> Dune_pkg.Solver_env.to_dyn |> Dyn.to_string |> fun s -> Sexp.Atom s
    in
    let constraints : Sexp.t =
      List
        (List.map constraints ~f:(fun ({ name; constraint_ } : Package_dependency.t) ->
           let name = Dune_lang.Package_name.to_string name in
           let constraint_ =
             match constraint_ with
             | None -> "no constraints"
             | Some c ->
               c |> Dune_pkg.Package_dependency.Constraint.to_dyn |> Dyn.to_string
           in
           Sexp.List [ Sexp.Atom name; Sexp.Atom constraint_ ]))
    in
    let selected_depopts : Sexp.t =
      List
        (List.map selected_depopts ~f:(fun pkg_name ->
           Sexp.Atom (Dune_lang.Package_name.to_string pkg_name)))
    in
    let pins =
      pins
      |> Dune_lang.Package_name.Map.to_list
      |> List.sort ~compare:(fun (a, _) (b, _) -> Dune_lang.Package_name.compare a b)
      |> List.map ~f:(fun (pkg_name, resolved_pkg) ->
        let name = Dune_lang.Package_name.to_string pkg_name in
        Sexp.List [ Sexp.Atom name; encode_resolved_pkg resolved_pkg ])
      |> fun xs -> Sexp.List xs
    in
    let version_preference =
      Sexp.List
        [ Sexp.Atom "version_preference"
        ; Sexp.Atom
            (match version_preference with
             | Oldest -> "oldest"
             | Newest -> "newest")
        ]
    in
    List
      [ encode_target target
      ; Sexp.Atom lock_dir
      ; packages
      ; repos
      ; env
      ; constraints
      ; selected_depopts
      ; pins
      ; version_preference
      ]
  ;;

  let action
        { target
        ; lock_dir = _
        ; packages
        ; repos
        ; env
        ; constraints
        ; selected_depopts
        ; pins
        ; version_preference
        }
        ~ectx:_
        ~eenv:_
    =
    let open Fiber.O in
    let* () = Fiber.return () in
    let local_packages =
      Package.Name.Map.map packages ~f:Dune_pkg.Local_package.for_solver
    in
    let portable_lock_dir = false in
    let* solver_result =
      Dune_pkg.Opam_solver.solve_lock_dir
        env
        version_preference
        repos
        ~pins
        ~local_packages
        ~constraints
        ~selected_depopts
        ~portable_lock_dir
    in
    match solver_result with
    | Error (`Diagnostic_message diagnostic) -> User_error.raise [ diagnostic ]
    | Ok { pinned_packages; files; lock_dir; _ } ->
      let lock_dir_path = Path.build target in
      let* lock_dir =
        Dune_pkg.Lock_dir.compute_missing_checksums ~pinned_packages lock_dir
      in
      (* Write lock directory using serialization from lock_dir.ml *)
      let file_contents =
        Dune_pkg.Lock_dir.file_contents_by_path ~portable_lock_dir lock_dir
      in
      Path.mkdir_p lock_dir_path;
      List.iter file_contents ~f:(fun (path_within_lock_dir, contents) ->
        let path = Path.relative lock_dir_path path_within_lock_dir in
        Option.iter (Path.parent path) ~f:Path.mkdir_p;
        let cst =
          List.map contents ~f:(fun sexp ->
            Dune_sexp.Ast.add_loc ~loc:Loc.none sexp |> Dune_sexp.Cst.concrete)
        in
        let pp = Dune_lang.Format.pp_top_sexps ~version:(3, 11) cst in
        Format.asprintf "%a" Pp.to_fmt pp |> Io.write_file path);
      (* Write .files directories *)
      Dune_pkg.Package_name.Map.iteri files ~f:(fun package_name files_by_version ->
        Dune_pkg.Package_version.Map.iteri
          files_by_version
          ~f:(fun package_version file_list ->
            let maybe_package_version =
              if portable_lock_dir then Some package_version else None
            in
            let files_dir =
              Dune_pkg.Lock_dir.Pkg.files_dir
                package_name
                maybe_package_version
                ~lock_dir:lock_dir_path
            in
            Path.mkdir_p files_dir;
            List.iter file_list ~f:(fun { Dune_pkg.File_entry.original; local_file } ->
              let dst = Path.append_local files_dir local_file in
              Path.mkdir_p (Path.parent_exn dst);
              match original with
              | Path src -> Io.copy_file ~src ~dst ()
              | Content content -> Io.write_file dst content)));
      Fiber.return ()
  ;;
end

module A = Action_ext.Make (Spec)

let repositories_of_workspace (workspace : Workspace.t) =
  List.map workspace.repos ~f:(fun repo ->
    Dune_pkg.Pkg_workspace.Repository.name repo, repo)
  |> Dune_pkg.Pkg_workspace.Repository.Name.Map.of_list_exn
;;

let constraints_of_workspace (workspace : Workspace.t) ~lock_dir_path =
  match Workspace.find_lock_dir workspace lock_dir_path with
  | None -> []
  | Some lock_dir -> lock_dir.constraints
;;

let depopts_of_workspace (workspace : Workspace.t) ~lock_dir_path =
  match Workspace.find_lock_dir workspace lock_dir_path with
  | None -> []
  | Some lock_dir -> lock_dir.depopts |> List.map ~f:snd
;;

let poll_solver_env_from_current_system () =
  Dune_pkg.Sys_poll.make ~path:(Env_path.path Stdune.Env.initial)
  |> Dune_pkg.Sys_poll.solver_env_from_current_system
;;

let project_and_package_pins project =
  let dir = Dune_project.root project in
  let pins = Dune_project.pins project in
  let packages = Dune_project.packages project in
  Dune_pkg.Pin.DB.add_opam_pins (Dune_pkg.Pin.DB.of_stanza ~dir pins) packages
;;

let resolve_project_pins project_pins =
  let scan_project ~read ~files =
    let read file = Memo.of_reproducible_fiber (read file) in
    (* Opam files may never contain recursive pins, so don't both reading them *)
    Dune_project.gen_load
      ~read
      ~files
      ~dir:Path.Source.root
      ~infer_from_opam_files:false
      ~load_opam_file_with_contents:Dune_pkg.Opam_file.load_opam_file_with_contents
    >>| Option.map ~f:(fun project ->
      let packages = Dune_project.packages project in
      let pins = project_and_package_pins project in
      pins, packages)
    |> Memo.run
  in
  Dune_pkg.Pin.resolve project_pins ~scan_project
;;

let project_pins =
  Dune_load.projects ()
  >>| List.fold_left ~init:Dune_pkg.Pin.DB.empty ~f:(fun acc project ->
    let pins = project_and_package_pins project in
    Dune_pkg.Pin.DB.combine_exn acc pins)
;;

let get_repos available_repos ~repositories =
  let module Repository = Dune_pkg.Pkg_workspace.Repository in
  let module Opam_repo = Dune_pkg.Opam_repo in
  repositories
  |> Fiber.parallel_map ~f:(fun (loc, name) ->
    match Repository.Name.Map.find available_repos name with
    | None ->
      User_error.raise
        ~loc
        [ Pp.textf "Repository '%s' is not a known repository"
          @@ Repository.Name.to_string name
        ]
    | Some repo ->
      let loc, opam_url = Repository.opam_url repo in
      (match Dune_pkg.OpamUrl.classify opam_url loc with
       | `Git -> Opam_repo.of_git_repo loc opam_url
       | `Path path -> Fiber.return @@ Opam_repo.of_opam_repo_dir_path loc path
       | `Archive ->
         User_error.raise
           ~loc
           [ Pp.textf "Repositories stored in archives (%s) are currently unsupported"
             @@ OpamUrl.to_string opam_url
           ]))
;;

let repo_state workspace lock_dir =
  let available_repos = repositories_of_workspace workspace in
  let default =
    Workspace.default_repositories
    |> List.map ~f:(fun repo ->
      let name = Dune_pkg.Pkg_workspace.Repository.name repo in
      Loc.none, name)
  in
  let repositories =
    (let open Option.O in
     let+ lock_dir = Workspace.find_lock_dir workspace lock_dir in
     lock_dir.repositories)
    |> Option.value ~default
  in
  Memo.of_non_reproducible_fiber (get_repos available_repos ~repositories)
;;

let env (lock_dir : Workspace.Lock_dir.t option) =
  let solver_env_from_context, unset_solver_vars_from_context =
    match lock_dir with
    | None -> None, None
    | Some lock_dir -> lock_dir.solver_env, lock_dir.unset_solver_vars
  in
  let+ solver_env_from_current_system =
    Memo.of_reproducible_fiber (poll_solver_env_from_current_system ()) >>| Option.some
  in
  solver_env
    ~solver_env_from_context
    ~solver_env_from_current_system
    ~unset_solver_vars_from_context
;;

let setup_lock_rules ~dir ~lock_dir : Gen_rules.result =
  let target = Path.Build.append_local dir lock_dir in
  let lock_dir_param = lock_dir in
  let rules =
    let+ workspace = Workspace.workspace () in
    let lock_dir_path = Path.of_local lock_dir_param in
    let lock_dir = Workspace.find_lock_dir workspace lock_dir_path in
    let constraints = constraints_of_workspace ~lock_dir_path workspace in
    let selected_depopts = depopts_of_workspace ~lock_dir_path workspace in
    let { Action_builder.With_targets.build; targets } =
      (let open Action_builder.O in
       let+ packages =
         let open Memo.O in
         Dune_load.packages ()
         >>| Dune_lang.Package.Name.Map.map ~f:Dune_pkg.Local_package.of_package
         |> Action_builder.of_memo
       and+ repos =
         Action_builder.of_memo
           (Memo.of_thunk (fun () ->
              let open Memo.O in
              let* workspace = Workspace.workspace () in
              let lock_dir_path = Path.of_local lock_dir_param in
              repo_state workspace lock_dir_path))
       and+ env =
         Action_builder.of_memo
           (Memo.of_thunk (fun () ->
              let open Memo.O in
              let* workspace = Workspace.workspace () in
              let lock_dir_path = Path.of_local lock_dir_param in
              let lock_dir_ws = Workspace.find_lock_dir workspace lock_dir_path in
              env lock_dir_ws))
       and+ pins =
         Action_builder.of_memo
           (Memo.of_thunk (fun () ->
              let open Memo.O in
              let* workspace = Workspace.workspace () in
              let* project_pins_db = project_pins in
              let workspace_pins_db =
                let workspace_pins = Dune_pkg.Pin.DB.Workspace.of_stanza workspace.pins in
                let pin_map = Dune_lang.Pin_stanza.Workspace.map workspace.pins in
                let all_pin_names =
                  pin_map
                  |> String.Map.to_list
                  |> List.fold_left ~init:[] ~f:(fun acc (_repo_name, pkg_map) ->
                    pkg_map
                    |> Dune_lang.Package_name.Map.to_list
                    |> List.fold_left
                         ~init:acc
                         ~f:(fun acc (pkg_name, ((loc, _url), _pkg)) ->
                           (loc, Dune_lang.Package_name.to_string pkg_name) :: acc))
                in
                Dune_pkg.Pin.DB.Workspace.extract workspace_pins ~names:all_pin_names
              in
              let combined_pins =
                Dune_pkg.Pin.DB.combine_exn workspace_pins_db project_pins_db
              in
              Memo.return combined_pins
              >>| resolve_project_pins
              >>= Memo.of_reproducible_fiber))
       in
       let version_preference =
         (let open Option.O in
          let* lock_dir = lock_dir in
          lock_dir.version_preference)
         |> Option.value ~default:Dune_pkg.Version_preference.default
       in
       A.action
         { Spec.target
         ; lock_dir = Path.to_string lock_dir_path
         ; packages
         ; repos
         ; env
         ; constraints
         ; selected_depopts
         ; pins
         ; version_preference
         }
       |> Action.Full.make ~can_go_in_shared_cache:false)
      |> Action_builder.with_no_targets
      |> Action_builder.With_targets.add_directories ~directory_targets:[ target ]
    in
    let rule = Rule.make ~targets build in
    Rules.of_rules [ rule ]
  in
  let directory_targets = Path.Build.Map.singleton target Loc.none in
  Gen_rules.make ~directory_targets rules
;;

let action_builder_with_dir_targets ~directory_targets =
  let targets =
    Targets.create
      ~files:Path.Build.Set.empty
      ~dirs:(Path.Build.Set.of_list directory_targets)
  in
  Action_builder.with_targets ~targets
;;

let copy_lock_dir ~target ~lock_dir ~deps ~files =
  let open Action_builder.O in
  Action_builder.deps deps
  >>> (Path.Set.to_list_map files ~f:(fun src ->
         let suffix = Path.drop_prefix_exn src ~prefix:(Path.source lock_dir) in
         let dst = Path.Build.append_local target suffix in
         let parent = Path.Build.parent_exn dst in
         Action.progn [ Action.mkdir parent; Action.copy src dst ])
       |> Action.concurrent
       |> Action.Full.make
       |> Action_builder.return)
  |> action_builder_with_dir_targets ~directory_targets:[ target ]
;;

let setup_copy_rules ~dir:target ~lock_dir =
  let+ deps, files = Source_deps.files (Path.source lock_dir) in
  let directory_targets, rules =
    match Path.Set.is_empty files with
    | true -> Path.Build.Map.empty, Rules.empty
    | false ->
      let directory_targets = Path.Build.Map.singleton target Loc.none in
      let { Action_builder.With_targets.build; targets } =
        copy_lock_dir ~target ~lock_dir ~deps ~files
      in
      let rule = Rule.make ~targets build in
      directory_targets, Rules.of_rules [ rule ]
  in
  Gen_rules.make ~directory_targets (Memo.return rules)
;;

let lock_dir_source (workspace : Workspace.t) ~lock_dir =
  let lock_dir = Path.Source.append_local workspace.dir lock_dir in
  let+ exists = Fs_memo.dir_exists (Path.Outside_build_dir.In_source_dir lock_dir) in
  match exists with
  | true -> `Source_tree lock_dir
  | false -> `Generated
;;

let setup_lock_rules_with_source workspace ~dir ~lock_dir =
  let* source = lock_dir_source workspace ~lock_dir in
  match source with
  | `Source_tree lock_dir ->
    let dir = Path.Build.append_source dir lock_dir in
    setup_copy_rules ~dir ~lock_dir
  | `Generated -> Memo.return (setup_lock_rules ~dir ~lock_dir)
;;

let setup_dev_tool_lock_rules ~dir dev_tool =
  let package_name = Dune_pkg.Dev_tool.package_name dev_tool in
  let dev_tool_name = Dune_lang.Package_name.to_string package_name in
  let dir = Path.Build.relative dir dev_tool_name in
  let lock_dir = Lock_dir.dev_tool_source_lock_dir dev_tool in
  setup_copy_rules ~dir ~lock_dir
;;

let lock_dirs_of_workspace (workspace : Workspace.t) =
  let module Set = Path.Source.Set in
  let+ lock_dirs_from_ctx =
    Memo.List.map workspace.contexts ~f:(function
      | Opam _ | Default { lock_dir = None; _ } -> Memo.return None
      | Default { lock_dir = Some selection; _ } ->
        let+ path = Lock_dir.select_lock_dir selection in
        Some path)
    >>| List.filter_opt
  in
  match lock_dirs_from_ctx, workspace.lock_dirs with
  | [], [] -> Set.singleton Lock_dir.default_source_path
  | lock_dirs_from_ctx, lock_dirs_from_toplevel ->
    let lock_paths_from_toplevel =
      List.map lock_dirs_from_toplevel ~f:(fun (lock_dir : Workspace.Lock_dir.t) ->
        lock_dir.path)
    in
    Set.union (Set.of_list lock_paths_from_toplevel) (Set.of_list lock_dirs_from_ctx)
;;

let setup_rules ~components ~dir =
  let empty = Gen_rules.rules_here Gen_rules.Rules.empty in
  match components with
  | [ ".lock" ] ->
    let* workspace = Workspace.workspace () in
    lock_dirs_of_workspace workspace
    >>| Path.Source.Set.to_list
    >>= Memo.List.fold_left ~init:empty ~f:(fun rules lock_dir_path ->
      let lock_dir = Path.Source.to_local lock_dir_path in
      let+ lock_rule = setup_lock_rules_with_source workspace ~dir ~lock_dir in
      Gen_rules.combine rules lock_rule)
  | [ ".dev-tool-locks" ] ->
    Memo.List.fold_left Dune_pkg.Dev_tool.all ~init:empty ~f:(fun rules dev_tool ->
      let+ dev_tool_rules = setup_dev_tool_lock_rules ~dir dev_tool in
      Gen_rules.combine rules dev_tool_rules)
  | [] ->
    let sub_dirs = [ ".lock"; ".dev-tool-locks" ] in
    let build_dir_only_sub_dirs =
      Gen_rules.Build_only_sub_dirs.singleton ~dir @@ Subdir_set.of_list sub_dirs
    in
    Memo.return @@ Gen_rules.make ~build_dir_only_sub_dirs (Memo.return Rules.empty)
  | _ -> Memo.return empty
;;
