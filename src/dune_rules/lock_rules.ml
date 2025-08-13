open Import
open Memo.O
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

let lock
      ~packages
      ~target
      ~lock_dir
      ~repos
      ~env
      ~constraints
      ~selected_depopts
      ~pins
      ~version_preference
  =
  A.action
    { Spec.target
    ; lock_dir
    ; packages
    ; repos
    ; env
    ; constraints
    ; selected_depopts
    ; pins
    ; version_preference
    }
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

let setup_lock_rules ctx_name ~dir ~lock_dir : Gen_rules.result =
  let target = Path.Build.relative dir "content" in
  let rules =
    Rules.collect_unit (fun () ->
      let* packages =
        Dune_load.packages ()
        >>| Dune_lang.Package.Name.Map.map ~f:Dune_pkg.Local_package.of_package
      in
      let* workspace = Workspace.workspace () in
      let repos = repositories_of_workspace workspace in
      let lock_dir_path =
        Path.Build.relative (Context_name.build_dir ctx_name) lock_dir
      in
      let constraints, selected_depopts =
        ( constraints_of_workspace ~lock_dir_path workspace
        , depopts_of_workspace ~lock_dir_path workspace )
      in
      let* repos =
        let default =
          Workspace.default_repositories
          |> List.map ~f:(fun repo ->
            let name = Dune_pkg.Pkg_workspace.Repository.name repo in
            Loc.none, name)
        in
        let repositories =
          (let open Option.O in
           let+ lock_dir = Workspace.find_lock_dir workspace lock_dir_path in
           lock_dir.repositories)
          |> Option.value ~default
        in
        Memo.of_non_reproducible_fiber (get_repos repos ~repositories)
      in
      let* solver_env_from_current_system =
        Memo.of_reproducible_fiber (poll_solver_env_from_current_system ())
        >>| Option.some
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
      let version_preference =
        (let open Option.O in
         let* lock_dir = Workspace.find_lock_dir workspace lock_dir_path in
         lock_dir.version_preference)
        |> Option.value ~default:Dune_pkg.Version_preference.default
      in
      let lock_rule =
        lock
          ~packages
          ~target
          ~lock_dir
          ~repos
          ~env
          ~constraints
          ~selected_depopts
          ~pins
          ~version_preference
      in
      rule ~loc:Loc.none lock_rule)
  in
  let directory_targets = Path.Build.Map.singleton target Loc.none in
  Gen_rules.make ~directory_targets rules
;;

let compiler_package_name = Dune_pkg.Package_name.of_string "ocaml"

(* Some dev tools must be built with the same version of the ocaml
   compiler as the project. This function returns the version of the
   "ocaml" package used to compile the project in the default build
   context.

   TODO: This only makes sure that the version of compiler used to
   build the dev tool matches the version of the compiler used to
   build this project. This will fail if the project is built with a
   custom compiler (e.g. ocaml-variants) since the version of the
   compiler will be the same between the project and dev tool while
   they still use different compilers. A more robust solution would be
   to ensure that the exact compiler package used to build the dev
   tool matches the package used to build the compiler. *)
let locked_ocaml_compiler_version () =
  let open Memo.O in
  let context =
    (* Dev tools are only ever built with the default context. *)
    Context_name.default
  in
  let* result = Lock_dir.get context
  and* platform = Solver_env.empty |> Memo.return in
  match result with
  | Error _ ->
    (* TODO better error or so *)
    Code_error.raise "Loading lock dir failed, something is up" []
  | Ok { packages; _ } ->
    let packages =
      Dune_pkg.Lock_dir.Packages.pkgs_on_platform_by_name packages ~platform
    in
    (match Package_name.Map.find packages compiler_package_name with
     | None ->
       User_error.raise
         [ Pp.textf
             "The lockdir doesn't contain a lockfile for the package %S."
             (Package_name.to_string compiler_package_name)
         ]
         ~hints:
           [ Pp.concat
               ~sep:Pp.space
               [ Pp.textf
                   "Add a dependency on %S to one of the packages in dune-project and \
                    then run"
                   (Package_name.to_string compiler_package_name)
               ; User_message.command "dune pkg lock"
               ]
           ]
     | Some pkg -> Memo.return pkg.info.version)
;;

let locked_ocaml_compiler_constraint () =
  let open Dune_lang in
  let open Memo.O in
  let+ ocaml_compiler_version = locked_ocaml_compiler_version () in
  let constraint_ =
    Some
      (Package_constraint.Uop
         (Eq, String_literal (Package_version.to_string ocaml_compiler_version)))
  in
  { Package_dependency.name = compiler_package_name; constraint_ }
;;

let extra_dependencies dev_tool =
  let open Memo.O in
  match Dune_pkg.Dev_tool.needs_to_build_with_same_compiler_as_project dev_tool with
  | false -> Memo.return []
  | true ->
    let+ constraint_ = locked_ocaml_compiler_constraint () in
    [ constraint_ ]
;;

(* Returns a version constraint accepting (almost) all versions whose prefix is
   the given version. This allows alternative distributions of packages to be
   chosen, such as choosing "ocamlformat.0.26.2+binary" when .ocamlformat
   contains "version=0.26.2". *)
let relaxed_version_constraint_of_version version =
  let open Dune_lang in
  let min_version = Package_version.to_string version in
  (* The goal here is to add a suffix to [min_version] to construct a version
     number higher than than any version number likely to appear with
     [min_version] as a prefix. "_" is the highest ascii symbol that can appear
     in version numbers, excluding "~" which has a special meaning. It's
     conceivable that one or two consecutive "_" characters may be used in a
     version, so this appends "___" to [min_version].

     Read more at: https://opam.ocaml.org/doc/Manual.html#Version-ordering
  *)
  let max_version = min_version ^ "___MAX_VERSION" in
  Package_constraint.And
    [ Package_constraint.Uop
        (Relop.Gte, Package_constraint.Value.String_literal min_version)
    ; Package_constraint.Uop
        (Relop.Lte, Package_constraint.Value.String_literal max_version)
    ]
;;

(* The solver satisfies dependencies for local packages, but dev tools
   are not local packages. As a workaround, create an empty local package
   which depends on the dev tool package. *)
let make_local_package_wrapping_dev_tool ~dev_tool ~dev_tool_version ~extra_dependencies
  : Dune_pkg.Local_package.t
  =
  let dev_tool_pkg_name = Dune_pkg.Dev_tool.package_name dev_tool in
  let dependency =
    let open Dune_lang in
    let open Package_dependency in
    let constraint_ =
      Option.map dev_tool_version ~f:relaxed_version_constraint_of_version
    in
    { name = dev_tool_pkg_name; constraint_ }
  in
  let local_package_name =
    Package_name.of_string (Package_name.to_string dev_tool_pkg_name ^ "_dev_tool_wrapper")
  in
  { Dune_pkg.Local_package.name = local_package_name
  ; version = Dune_pkg.Lock_dir.Pkg_info.default_version
  ; dependencies =
      Dune_pkg.Dependency_formula.of_dependencies (dependency :: extra_dependencies)
  ; conflicts = []
  ; depopts = []
  ; pins = Package_name.Map.empty
  ; conflict_class = []
  ; loc = Loc.none
  ; command_source = Opam_file { build = []; install = [] }
  }
;;

(* [lock_dev_tool_at_version ~target dev_tool version] generates the lockdir
   for the dev tool [dev_tool]. If [version] is [Some v] then version [v] of
   the tool will be chosen by the solver. Otherwise the solver is free to
   choose the appropriate version of the tool to install. *)
let lock_dev_tool_at_version ctx_name ~target dev_tool version =
  let open Memo.O in
  let* extra_dependencies = extra_dependencies dev_tool in
  let local_pkg =
    make_local_package_wrapping_dev_tool
      ~dev_tool
      ~dev_tool_version:version
      ~extra_dependencies
  in
  let packages = Package_name.Map.singleton local_pkg.name local_pkg in
  let constraints = [] in
  let lock_dir = sprintf "dev-tools.locks/%s" (Dune_pkg.Dev_tool.to_string dev_tool) in
  let* workspace = Workspace.workspace () in
  let repos = repositories_of_workspace workspace in
  let lock_dir_path = Path.Build.relative (Context_name.build_dir ctx_name) lock_dir in
  let* repos =
    let default =
      Workspace.default_repositories
      |> List.map ~f:(fun repo ->
        let name = Dune_pkg.Pkg_workspace.Repository.name repo in
        Loc.none, name)
    in
    let repositories =
      (let open Option.O in
       let+ lock_dir = Workspace.find_lock_dir workspace lock_dir_path in
       lock_dir.repositories)
      |> Option.value ~default
    in
    Memo.of_non_reproducible_fiber (get_repos repos ~repositories)
  in
  let selected_depopts = [] in
  let pins = Package_name.Map.empty in
  let version_preference = Dune_pkg.Version_preference.default in
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
  let lock_rule =
    lock
      ~packages
      ~target
      ~lock_dir
      ~constraints
      ~repos
      ~env
      ~selected_depopts
      ~pins
      ~version_preference
  in
  rule ~loc:Loc.none lock_rule
;;

let lock_ocamlformat ctx_name ~target =
  let version = Dune_pkg.Ocamlformat.version_of_current_project's_ocamlformat_config () in
  lock_dev_tool_at_version ctx_name ~target Ocamlformat version
;;

let setup_dev_tool_lock_rules ctx_name ~dir ~dev_tool : Gen_rules.result =
  let target = Path.Build.relative dir "content" in
  let directory_targets = Path.Build.Map.singleton target Loc.none in
  let rules =
    Rules.collect_unit (fun () ->
      match (dev_tool : Dune_pkg.Dev_tool.t) with
      | Ocamlformat -> lock_ocamlformat ctx_name ~target
      | other -> lock_dev_tool_at_version ctx_name ~target other None)
  in
  Gen_rules.make ~directory_targets rules
;;

module Copy = struct
  module Spec = struct
    type ('path, 'target) t =
      { target : 'target
      ; lock_dir : Path.Source.t
      ; contents : Fs_cache.Dir_contents.t
      }

    let name = "copy-lock-dir"
    let version = 1
    let bimap t _ g = { t with target = g t.target }
    let is_useful_to ~memoize = memoize

    let encode { target; lock_dir; contents } _encode_path encode_target : Sexp.t =
      let contents : Sexp.t list =
        contents
        |> Fs_cache.Dir_contents.to_list
        |> List.map ~f:(fun (filename, filekind) ->
          Sexp.List
            [ Atom filename; Atom (filekind |> File_kind.to_dyn |> Dyn.to_string) ])
      in
      List [ encode_target target; Atom (Path.Source.to_string lock_dir); List contents ]
    ;;

    let action { target; lock_dir; contents } ~ectx:_ ~eenv:_ =
      let open Fiber.O in
      let+ () = Fiber.return () in
      Path.mkdir_p (Path.build target);
      Fs_cache.Dir_contents.iter contents ~f:(fun (filename, _file_kind) ->
        let src = Path.Source.relative lock_dir filename in
        let dst = Path.Build.relative target filename in
        Io.copy_file ~src:(Path.source src) ~dst:(Path.build dst) ())
    ;;
  end

  module A = Action_ext.Make (Spec)
end

let copy_lock_dir ~target ~lock_dir ~contents =
  Copy.A.action { Copy.Spec.target; lock_dir; contents }
  |> Action.Full.make ~can_go_in_shared_cache:false
  |> Action_builder.With_targets.return
  |> Action_builder.With_targets.add_directories ~directory_targets:[ target ]
;;

let setup_copy_rules _ctx_name ~dir ~lock_dir =
  let target = Path.Build.relative dir "content" in
  let rules =
    Rules.collect_unit (fun () ->
      let lock_dir = Path.Source.of_string lock_dir in
      let obd = Path.Outside_build_dir.In_source_dir lock_dir in
      let* dir_contents = Fs_memo.dir_contents obd in
      let contents =
        match dir_contents with
        | Error _ -> User_error.raise [ Pp.text "eeek" ]
        | Ok dir_contents -> dir_contents
      in
      let copy_rule = copy_lock_dir ~target ~lock_dir ~contents in
      rule ~loc:Loc.none copy_rule)
  in
  let directory_targets = Path.Build.Map.singleton target Loc.none in
  Gen_rules.make ~directory_targets rules
;;

let lock_dir_source ~lock_dir =
  (* TODI determine this from workspace *)
  let expected_path_in_source = Path.Outside_build_dir.of_string lock_dir in
  let+ exists = Fs_memo.dir_exists expected_path_in_source in
  match exists with
  | true -> `Source_tree
  | false -> `Generated
;;

let setup_lock_rules ctx_name ~dir ~lock_dir =
  let+ source = lock_dir_source ~lock_dir in
  match source with
  | `Source_tree -> setup_copy_rules ctx_name ~dir ~lock_dir
  | `Generated -> setup_lock_rules ctx_name ~dir ~lock_dir
;;

let setup_rules ctx_name ~components ~dir =
  match components with
  | [ ".dev-tool-lock"; tool ] ->
    let dev_tool =
      match Dune_pkg.Dev_tool.of_string tool with
      | Some dev_tool -> dev_tool
      | None -> User_error.raise [ Pp.textf "No rules for dev tool %s" tool ]
    in
    Memo.return @@ setup_dev_tool_lock_rules ctx_name ~dir ~dev_tool
  | [ ".lock" ] | [ ".dev-tool-lock" ] ->
    Gen_rules.make
      ~build_dir_only_sub_dirs:
        (Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
      (Memo.return Rules.empty)
    |> Memo.return
  | [ ".lock"; lock_dir ] -> setup_lock_rules ctx_name ~dir ~lock_dir
  | [] ->
    let sub_dirs = [ ".lock"; ".dev-tool-lock" ] in
    let build_dir_only_sub_dirs =
      Gen_rules.Build_only_sub_dirs.singleton ~dir @@ Subdir_set.of_list sub_dirs
    in
    Memo.return @@ Gen_rules.make ~build_dir_only_sub_dirs (Memo.return Rules.empty)
  | _ -> Memo.return @@ Gen_rules.rules_here Gen_rules.Rules.empty
;;

let setup_lock_alias =
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

(* TODO remove this, just for debug purposes *)
let setup_tmp_ocamlformat_alias =
  fun ~dir ctx_name ->
  let alias = Alias.make ~dir Alias0.pkg_tmp_ocamlformat_lock in
  let rule =
    Rules.collect_unit (fun () ->
      (* careful, need to point to a file that will be created by the rule *)
      let path =
        (* TODO get lock dir name instead of hardcoding `dune.lock` *)
        Path.Build.L.relative
          Private_context.t.build_dir
          [ Context_name.to_string ctx_name; ".dev-tool-lock"; "ocamlformat"; "content" ]
      in
      let deps = Action_builder.path (Path.build path) in
      Rules.Produce.Alias.add_deps alias deps)
  in
  Gen_rules.rules_for ~dir ~allowed_subdirs:Filename.Set.empty rule
  |> Gen_rules.rules_here
;;
