open Import
open Memo.O

module Spec = struct
  type ('path, 'target) t =
    { target : 'target
    ; lock_dir : string
    ; packages : Dune_pkg.Local_package.t Package.Name.Map.t
    ; repos : Dune_pkg.Opam_repo.t list
    }

  let name = "lock"
  let version = 1
  let bimap t _ g = { t with target = g t.target }
  let is_useful_to ~memoize = memoize

  let encode { target; lock_dir; packages; repos } _encode_path encode_target : Sexp.t =
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
    List [ encode_target target; Sexp.Atom lock_dir; packages; repos ]
  ;;

  let action { target; lock_dir; packages; repos } ~ectx:_ ~eenv:_ =
    let open Fiber.O in
    let* () = Fiber.return () in
    Printf.eprintf
      "Running ACTION, target is %s, our lock_dir is %S\n"
      (Path.Build.to_string target)
      lock_dir;
    let path = Path.build target in
    Path.mkdir_p path;
    let version_preference = Dune_pkg.Version_preference.default in
    let local_packages =
      Package.Name.Map.map packages ~f:Dune_pkg.Local_package.for_solver
    in
    (* TODO: add values to action *)
    let env = Dune_pkg.Solver_env.empty in
    let pins = Package.Name.Map.empty in
    let constraints = [] in
    let selected_depopts = [] in
    let portable_lock_dir =
      match Config.get Compile_time.portable_lock_dir with
      | `Enabled -> true
      | `Disabled -> false
    in
    let+ solver_result =
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
    let _solver_result =
      match solver_result with
      | Ok success ->
        Printf.eprintf "Solver found solution, TODO: write lock dir\n";
        success
      | Error (`Diagnostic_message diagnostic) -> User_error.raise [ diagnostic ]
    in
    let content =
      Package.Name.Map.values packages
      |> List.map ~f:(fun (s : Dune_pkg.Local_package.t) ->
        s.name |> Package.Name.to_dyn |> Dyn.to_string)
      |> String.concat ~sep:"\n"
    in
    Io.write_file ~binary:true (Path.relative path "lock.dune") content
  ;;
end

module A = Action_ext.Make (Spec)

let lock ~packages ~target ~lock_dir ~repos =
  A.action { Spec.target; lock_dir; packages; repos }
  |> Action.Full.make ~can_go_in_shared_cache:false
  |> Action_builder.With_targets.return
  |> Action_builder.With_targets.add_directories ~directory_targets:[ target ]
;;

module Gen_rules = Build_config.Gen_rules

let rule ?loc { Action_builder.With_targets.build; targets } =
  (* TODO this ignores the workspace file *)
  Rule.make ~info:(Rule.Info.of_loc_opt loc) ~targets build |> Rules.Produce.rule
;;

let repositories_of_workspace (workspace : Workspace.t) =
  List.map workspace.repos ~f:(fun repo ->
    Dune_pkg.Pkg_workspace.Repository.name repo, repo)
  |> Dune_pkg.Pkg_workspace.Repository.Name.Map.of_list_exn
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

let setup_lock_rules ~dir ~lock_dir : Gen_rules.result =
  let target = Path.Build.relative dir "content" in
  let rules =
    Rules.collect_unit (fun () ->
      let* packages =
        Dune_load.packages ()
        >>| Dune_lang.Package.Name.Map.map ~f:Dune_pkg.Local_package.of_package
      in
      let* workspace = Workspace.workspace () in
      let repos = repositories_of_workspace workspace in
      let* repos =
        (* TODO: read from the right place instead of hardcoding here *)
        Memo.of_non_reproducible_fiber
          (get_repos
             repos
             ~repositories:
               [ Loc.none, Dune_pkg.Pkg_workspace.Repository.Name.of_string "upstream" ])
      in
      let lock_rule = lock ~packages ~target ~lock_dir ~repos in
      rule ~loc:Loc.none lock_rule)
  in
  let directory_targets = Path.Build.Map.singleton target Loc.none in
  Gen_rules.make ~directory_targets rules
;;

let setup_rules ~components ~dir =
  match components with
  | [ ".lock" ] ->
    Gen_rules.make
      ~build_dir_only_sub_dirs:
        (Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
      (Memo.return Rules.empty)
    |> Memo.return
  | [ ".lock"; lock_dir ] -> Memo.return @@ setup_lock_rules ~dir ~lock_dir
  | [] ->
    let sub_dirs = [ ".lock" ] in
    let build_dir_only_sub_dirs =
      Gen_rules.Build_only_sub_dirs.singleton ~dir @@ Subdir_set.of_list sub_dirs
    in
    Memo.return @@ Gen_rules.make ~build_dir_only_sub_dirs (Memo.return Rules.empty)
  | _ -> Memo.return @@ Gen_rules.rules_here Gen_rules.Rules.empty
;;

let setup_tmp_lock_alias =
  fun ~dir ctx_name ->
  let alias = Alias.make ~dir Alias0.pkg_lock in
  let rule =
    Rules.collect_unit (fun () ->
      (* careful, need to point to a file that will be created by the rule *)
      let path =
        (* TODO get lock dir name instead of hardcoding `dune.lock` *)
        Path.Build.L.relative
          Private_context.t.build_dir
          [ Context_name.to_string ctx_name; ".lock"; "dune.lock"; "content" ]
      in
      let deps = Action_builder.path (Path.build path) in
      Rules.Produce.Alias.add_deps alias deps)
  in
  Gen_rules.rules_for ~dir ~allowed_subdirs:Filename.Set.empty rule
  |> Gen_rules.rules_here
;;
