open Import
open Memo.O
module Solver_env = Dune_pkg.Solver_env

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
    }

  let name = "lock"
  let version = 1
  let bimap t _ g = { t with target = g t.target }
  let is_useful_to ~memoize = memoize

  let encode
        { target; lock_dir; packages; repos; env; constraints; selected_depopts; pins }
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
           repo |> Dune_pkg.Opam_repo.to_dyn |> Dyn.to_string |> fun s -> Sexp.Atom s))
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
      |> List.map ~f:(fun (pkg_name, _resolved_pkg) ->
        let name = Dune_lang.Package_name.to_string pkg_name in
        Sexp.List [ Sexp.Atom name; Sexp.Atom "TODO: representation of resolved pkg" ])
      |> fun xs -> Sexp.List xs
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
        }
        ~ectx:_
        ~eenv:_
    =
    let open Fiber.O in
    let* () = Fiber.return () in
    let version_preference = Dune_pkg.Version_preference.default in
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
      let+ lock_dir =
        Dune_pkg.Lock_dir.compute_missing_checksums ~pinned_packages lock_dir
      in
      Dune_pkg.Lock_dir.Write_disk.prepare
        ~portable_lock_dir
        ~lock_dir_path
        ~files
        lock_dir
      |> Dune_pkg.Lock_dir.Write_disk.commit
  ;;
end

module A = Action_ext.Make (Spec)

let lock ~packages ~target ~lock_dir ~repos ~env ~constraints ~selected_depopts ~pins =
  A.action
    { Spec.target; lock_dir; packages; repos; env; constraints; selected_depopts; pins }
  |> Action.Full.make ~can_go_in_shared_cache:false
  |> Action_builder.With_targets.return
  |> Action_builder.With_targets.add_directories ~directory_targets:[ target ]
;;

module Gen_rules = Build_config.Gen_rules

let rule ?loc { Action_builder.With_targets.build; targets } =
  Rule.make ~info:(Rule.Info.of_loc_opt loc) ~targets build |> Rules.Produce.rule
;;

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

let get_repos repos ~repositories =
  let module Repository = Dune_pkg.Pkg_workspace.Repository in
  repositories
  |> Fiber.parallel_map ~f:(fun (loc, name) ->
    match Repository.Name.Map.find repos name with
    | None ->
      User_error.raise
        ~loc
        [ Pp.textf "Repository '%s' is not a known repository"
          @@ Repository.Name.to_string name
        ]
    | Some repo ->
      let loc, opam_url = Repository.opam_url repo in
      let module Opam_repo = Dune_pkg.Opam_repo in
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

let setup_tmp_lock_alias =
  fun ~dir ctx_name ->
  let alias = Alias.make ~dir Alias0.pkg_lock in
  let rule =
    Rules.collect_unit (fun () ->
      (* careful, need to point to a file that will be created by the rule *)
      let path =
        (* TODO get lock dir name instead of hardcoding `dune.lock` *)
        Path.Build.relative (Context_name.build_dir ctx_name) "dune.lock"
      in
      let deps = Action_builder.path (Path.build path) in
      Rules.Produce.Alias.add_deps alias deps)
  in
  Gen_rules.rules_for ~dir ~allowed_subdirs:Filename.Set.empty rule
  |> Gen_rules.rules_here
;;

let rules _sctx ~dir =
  Printf.eprintf "Generating rules for dir: %s\n" (Path.Build.to_string dir);
  (* TODO: generate them for all lock dirs not just "dune.lock" *)
  let lock_dir = "dune.lock" in
  let target = Path.Build.relative dir lock_dir in
  let* packages =
    Dune_load.packages ()
    >>| Dune_lang.Package.Name.Map.map ~f:Dune_pkg.Local_package.of_package
  in
  let* workspace = Workspace.workspace () in
  let repos = repositories_of_workspace workspace in
  let constraints, selected_depopts =
    let lock_dir_path = target in
    ( constraints_of_workspace ~lock_dir_path workspace
    , depopts_of_workspace ~lock_dir_path workspace )
  in
  let* repos =
    (* TODO: read from the right place instead of hardcoding here *)
    Memo.of_non_reproducible_fiber
      (get_repos
         repos
         ~repositories:
           [ Loc.none, Dune_pkg.Pkg_workspace.Repository.Name.of_string "upstream" ])
  in
  let* solver_env_from_current_system =
    Memo.of_reproducible_fiber (poll_solver_env_from_current_system ()) >>| Option.some
  in
  let env =
    (* TODO read context and other stuff *)
    solver_env
      ~solver_env_from_context:None
      ~solver_env_from_current_system
      ~unset_solver_vars_from_context:None
  in
  let* project_pins = project_pins in
  let* pins = Memo.of_reproducible_fiber (resolve_project_pins project_pins) in
  let lock_rule =
    lock ~packages ~target ~lock_dir ~repos ~env ~constraints ~selected_depopts ~pins
  in
  rule ~loc:Loc.none lock_rule
;;
