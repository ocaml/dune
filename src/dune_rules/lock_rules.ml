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

let project_and_package_pins project =
  let dir = Dune_project.root project in
  let pins = Dune_project.pins project in
  let packages = Dune_project.packages project in
  Pin.DB.add_opam_pins (Pin.DB.of_stanza ~dir pins) packages
;;

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
       Lock_action.action
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
