open Import
open Memo.O
module Gen_rules = Build_config.Gen_rules

include struct
  open Dune_pkg
  module Solver_env = Solver_env
  module Package_name = Package_name
  module Opam_repo = Opam_repo
  module Local_package = Local_package
  module Resolved_package = Resolved_package
  module Version_preference = Version_preference
  module Package_universe = Package_universe
  module Package_dependency = Package_dependency
  module Opam_solver = Opam_solver
  module Pin = Pin
  module Opam_file = Opam_file
  module Sys_poll = Sys_poll
  module Pkg_workspace = Pkg_workspace
  module OpamUrl = OpamUrl
  module Dev_tool = Dev_tool
end

module Spec = struct
  type ('path, 'target) t =
    { target : 'target
    ; lock_dir : 'path
    ; packages : Local_package.t Package.Name.Map.t
    ; repos : Opam_repo.t list
    ; solver_env_from_context : Solver_env.t
    ; unset_solver_vars : Package_variable_name.Set.t
    ; constraints : Package_dependency.t list
    ; selected_depopts : Package.Name.t list
    ; pins : Resolved_package.t Package.Name.Map.t
    ; version_preference : Version_preference.t
    }

  let name = "lock"
  let version = 1
  let bimap t f g = { t with lock_dir = f t.lock_dir; target = g t.target }
  let is_useful_to ~memoize = memoize

  let encode
        { target
        ; lock_dir
        ; packages
        ; repos
        ; solver_env_from_context
        ; unset_solver_vars
        ; constraints
        ; selected_depopts
        ; pins
        ; version_preference
        }
        encode_path
        encode_target
    =
    Sexp.record
      [ "target", encode_target target
      ; "lock_dir", encode_path lock_dir
      ; ( "packages"
        , match Package_universe.dependency_digest packages with
          | None -> Atom "no packages"
          | Some hash ->
            List [ Atom "hash"; Atom (Local_package.Dependency_hash.to_string hash) ] )
      ; ( "repos"
        , List
            (List.map repos ~f:(fun repo ->
               Sexp.Atom (Opam_repo.content_digest repo |> Dune_digest.to_string))) )
      ; ( "solver_env_from_context"
        , Atom
            (Dune_digest.Feed.compute_digest
               Solver_env.digest_feed
               solver_env_from_context
             |> Dune_digest.to_string) )
      ; ( "unset_solver_vars"
        , List
            (Package_variable_name.Set.to_list unset_solver_vars
             |> List.sort ~compare:Package_variable_name.compare
             |> List.map ~f:(fun var -> Sexp.Atom (Package_variable_name.to_string var)))
        )
      ; ( "constraints"
        , List
            (List.sort constraints ~compare:(fun a b ->
               Dune_lang.Package_name.compare
                 a.Package_dependency.name
                 b.Package_dependency.name)
             |> List.map ~f:(fun { Package_dependency.name; constraint_ } ->
               let name = Dune_lang.Package_name.to_string name in
               let constraint_ =
                 match constraint_ with
                 | None -> "no constraints"
                 | Some c -> Package_dependency.Constraint.to_dyn c |> Dyn.to_string
               in
               Sexp.List [ Sexp.Atom name; Sexp.Atom constraint_ ])) )
      ; ( "selected_depopts"
        , List
            (List.sort selected_depopts ~compare:Dune_lang.Package_name.compare
             |> List.map ~f:(fun pkg_name ->
               Sexp.Atom (Dune_lang.Package_name.to_string pkg_name))) )
      ; ( "pins"
        , List
            (Dune_lang.Package_name.Map.to_list pins
             |> List.sort ~compare:(fun (a, _) (b, _) ->
               Dune_lang.Package_name.compare a b)
             |> List.map ~f:(fun (pkg_name, resolved_pkg) ->
               let name = Dune_lang.Package_name.to_string pkg_name in
               let digest =
                 Resolved_package.digest resolved_pkg |> Dune_digest.to_string
               in
               Sexp.List [ Sexp.Atom name; Sexp.Atom digest ])) )
      ; ( "version_preference"
        , Atom
            (match version_preference with
             | Oldest -> "oldest"
             | Newest -> "newest") )
      ]
  ;;

  let action
        { target
        ; lock_dir = _
        ; packages
        ; repos
        ; solver_env_from_context
        ; unset_solver_vars
        ; constraints
        ; selected_depopts
        ; pins
        ; version_preference
        }
        ~ectx:_
        ~eenv:{ Action.Ext.Exec.env; _ }
    =
    let open Fiber.O in
    let* () = Fiber.return () in
    let local_packages = Package.Name.Map.map packages ~f:Local_package.for_solver in
    (* Whether or not the lock directory we are creating is portable or not
       doesn't concern us. We therefore set it as non-portable. *)
    let portable_lock_dir = false in
    let* solver_env =
      (* CR-soon Alizter: This solver environment construction pattern (combining
       solver_env_from_current_system with solver_env_from_context, then
       unsetting vars) is similar to logic in bin/pkg/pkg_common.ml:solver_env
       and bin/pkg/lock.ml. Consider sharing this pattern. *)
      let open Fiber.O in
      let+ solver_env_from_current_system =
        Sys_poll.make ~path:(Env_path.path env) |> Sys_poll.solver_env_from_current_system
      in
      let solver_env =
        [ solver_env_from_current_system; solver_env_from_context ]
        |> List.fold_left ~init:Solver_env.with_defaults ~f:Solver_env.extend
      in
      Solver_env.unset_multi solver_env unset_solver_vars
    in
    let* solver_result =
      Opam_solver.solve_lock_dir
        solver_env
        version_preference
        repos
        ~pins
        ~local_packages
        ~constraints
        ~selected_depopts
        ~portable_lock_dir
    in
    match solver_result with
    | Error (`Manifest_error diagnostic) -> raise (User_error.E diagnostic)
    | Error (`Solve_error diagnostic) -> User_error.raise [ diagnostic ]
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

let lock_action
      ~target
      ~lock_dir
      ~packages
      ~repos
      ~solver_env_from_context
      ~unset_solver_vars
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
    ; solver_env_from_context
    ; unset_solver_vars
    ; constraints
    ; selected_depopts
    ; pins
    ; version_preference
    }
;;

let project_and_package_pins project =
  let dir = Dune_project.root project in
  let pins = Dune_project.pins project in
  let packages = Dune_project.packages project in
  Pin.DB.add_opam_pins (Pin.DB.of_stanza ~dir pins) packages
;;

(* CR-soon Alizter: This function is duplicated in bin/pkg/lock.ml. We should
   factor out pin handling logic into a shared module in dune_pkg. *)
let resolve_project_pins project_pins =
  let scan_project ~read ~files =
    let read file = Memo.of_reproducible_fiber (read file) in
    Dune_project.gen_load
      ~read
      ~files
      ~dir:Path.Source.root
      ~infer_from_opam_files:false
      ~load_opam_file_with_contents:Opam_file.load_opam_file_with_contents
    >>| Option.map ~f:(fun project ->
      let packages = Dune_project.packages project in
      let pins = project_and_package_pins project in
      pins, packages)
    |> Memo.run
  in
  Pin.resolve project_pins ~scan_project
;;

(* CR-soon Alizter: This function is duplicated in bin/pkg/lock.ml. We should
   factor out pin handling logic into a shared module in dune_pkg. *)
let project_pins =
  Dune_load.projects ()
  >>| List.fold_left ~init:Pin.DB.empty ~f:(fun acc project ->
    let pins = project_and_package_pins project in
    Pin.DB.combine_exn acc pins)
;;

let setup_lock_rules ~dir ~lock_dir : Gen_rules.result =
  let target = Path.Build.append_local dir lock_dir in
  let lock_dir_param = lock_dir in
  let rules =
    let+ workspace = Workspace.workspace () in
    let lock_dir_path = Path.of_local lock_dir_param in
    let lock_dir = Workspace.find_lock_dir workspace lock_dir_path in
    let constraints =
      match lock_dir with
      | None -> []
      | Some lock_dir -> lock_dir.constraints
    in
    let selected_depopts =
      match lock_dir with
      | None -> []
      | Some lock_dir -> lock_dir.depopts |> List.map ~f:snd
    in
    let { Action_builder.With_targets.build; targets } =
      (let open Action_builder.O in
       let+ packages =
         let open Memo.O in
         Dune_load.packages ()
         >>| Dune_lang.Package.Name.Map.map ~f:Local_package.of_package
         |> Action_builder.of_memo
       and+ repos =
         (* CR-soon Alizter: This repository handling logic is duplicated in
            bin/pkg/pkg_common.ml:get_repos. The OpamUrl.classify pattern
            matching and repository resolution could be shared. *)
         Action_builder.of_memo
           (Memo.of_thunk (fun () ->
              let repositories =
                let default =
                  Workspace.default_repositories
                  |> List.map ~f:(fun repo ->
                    let name = Pkg_workspace.Repository.name repo in
                    Loc.none, name)
                in
                (let open Option.O in
                 let+ lock_dir = Workspace.find_lock_dir workspace lock_dir_path in
                 lock_dir.repositories)
                |> Option.value ~default
              in
              let available_repos =
                List.map workspace.repos ~f:(fun repo ->
                  Pkg_workspace.Repository.name repo, repo)
                |> Pkg_workspace.Repository.Name.Map.of_list_exn
              in
              let module Repository = Pkg_workspace.Repository in
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
                  (match OpamUrl.classify opam_url loc with
                   | `Git -> Opam_repo.of_git_repo loc opam_url
                   | `Path path ->
                     Fiber.return @@ Opam_repo.of_opam_repo_dir_path loc path
                   | `Archive ->
                     User_error.raise
                       ~loc
                       [ Pp.textf
                           "Repositories stored in archives (%s) are currently \
                            unsupported"
                         @@ OpamUrl.to_string opam_url
                       ]))
              |> Memo.of_non_reproducible_fiber))
       and+ pins =
         (* CR-soon Alizter: This pin logic (extracting workspace pins,
            combining with project pins) is duplicated in bin/pkg/lock.ml. The
            pattern of Pin.DB.Workspace.extract and Pin.DB.combine_exn could be
            factored into a shared helper. *)
         Action_builder.of_memo
           (Memo.of_thunk (fun () ->
              let open Memo.O in
              let* project_pins_db = project_pins in
              let workspace_pins_db =
                let workspace_pins = Pin.DB.Workspace.of_stanza workspace.pins in
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
                Pin.DB.Workspace.extract workspace_pins ~names:all_pin_names
              in
              let combined_pins = Pin.DB.combine_exn workspace_pins_db project_pins_db in
              Memo.return combined_pins
              >>| resolve_project_pins
              >>= Memo.of_reproducible_fiber))
       in
       let version_preference =
         match lock_dir with
         | None -> Version_preference.default
         | Some { version_preference = None; _ } -> Version_preference.default
         | Some { version_preference = Some vp; _ } -> vp
       in
       let solver_env_from_context =
         match lock_dir with
         | None -> Solver_env.with_defaults
         | Some { solver_env = None; _ } -> Solver_env.with_defaults
         | Some { solver_env = Some env; _ } ->
           Solver_env.extend Solver_env.with_defaults env
       in
       let unset_solver_vars =
         match lock_dir with
         | None -> Package_variable_name.Set.empty
         | Some { unset_solver_vars = None; _ } -> Package_variable_name.Set.empty
         | Some { unset_solver_vars = Some vars; _ } -> vars
       in
       lock_action
         ~target
         ~lock_dir:lock_dir_path
         ~packages
         ~repos
         ~solver_env_from_context
         ~unset_solver_vars
         ~constraints
         ~selected_depopts
         ~pins
         ~version_preference
       |> Action.Full.make
            ~can_go_in_shared_cache:false (* TODO: probably ok this allow this? *)
            ~sandbox:Sandbox_config.needs_sandboxing)
      |> Action_builder.with_no_targets
      |> Action_builder.With_targets.add_directories ~directory_targets:[ target ]
    in
    let rule = Rule.make ~targets build in
    Rules.of_rules [ rule ]
  in
  let directory_targets = Path.Build.Map.singleton target Loc.none in
  Gen_rules.make ~directory_targets rules
;;

let copy_lock_dir ~target ~lock_dir ~deps ~files =
  let open Action_builder.O in
  Action_builder.deps deps
  >>> (Path.Set.to_list_map files ~f:(fun src ->
         let dst =
           Path.drop_prefix_exn src ~prefix:(Path.source lock_dir)
           |> Path.Build.append_local target
         in
         Action.progn [ Action.mkdir (Path.Build.parent_exn dst); Action.copy src dst ])
       |> Action.concurrent
       |> Action.Full.make
       |> Action_builder.return)
  |> Action_builder.with_targets
       ~targets:
         (Targets.create
            ~files:Path.Build.Set.empty
            ~dirs:(Path.Build.Set.singleton target))
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

let setup_lock_rules_with_source (workspace : Workspace.t) ~dir ~lock_dir =
  let* source =
    let lock_dir = Path.Source.append_local workspace.dir lock_dir in
    let+ exists = Fs_memo.dir_exists (Path.Outside_build_dir.In_source_dir lock_dir) in
    match exists with
    | true -> `Source_tree lock_dir
    | false -> `Generated
  in
  match source with
  | `Source_tree lock_dir ->
    let dir = Path.Build.append_source dir lock_dir in
    setup_copy_rules ~dir ~lock_dir
  | `Generated -> Memo.return (setup_lock_rules ~dir ~lock_dir)
;;

let setup_dev_tool_lock_rules ~dir dev_tool =
  let package_name = Dev_tool.package_name dev_tool in
  let dev_tool_name = Dune_lang.Package_name.to_string package_name in
  let dir = Path.Build.relative dir dev_tool_name in
  let lock_dir = Lock_dir.dev_tool_source_lock_dir dev_tool in
  setup_copy_rules ~dir ~lock_dir
;;

let setup_rules ~components ~dir =
  let empty = Gen_rules.rules_here Gen_rules.Rules.empty in
  match components with
  | [ ".lock" ] ->
    let* workspace = Workspace.workspace () in
    Lock_dir.lock_dirs_of_workspace workspace
    >>| Path.Source.Set.to_list
    >>= Memo.List.fold_left ~init:empty ~f:(fun rules lock_dir_path ->
      let lock_dir = Path.Source.to_local lock_dir_path in
      let+ lock_rule = setup_lock_rules_with_source workspace ~dir ~lock_dir in
      Gen_rules.combine rules lock_rule)
  | [ ".dev-tool-locks" ] ->
    Memo.List.fold_left Dev_tool.all ~init:empty ~f:(fun rules dev_tool ->
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
