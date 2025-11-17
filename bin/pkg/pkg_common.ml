open Import
module Lock_dir = Dune_pkg.Lock_dir
module Solver_env = Dune_pkg.Solver_env
module Package_variable_name = Dune_lang.Package_variable_name
module Variable_value = Dune_pkg.Variable_value

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

let poll_solver_env_from_current_system () =
  Dune_pkg.Sys_poll.make ~path:(Env_path.path Stdune.Env.initial)
  |> Dune_pkg.Sys_poll.solver_env_from_current_system
;;

let get_lock_dir_from_context ~lock_dir_path =
  Memo.run
  @@
  let open Memo.O in
  let+ workspace = Workspace.workspace () in
  Workspace.find_lock_dir workspace lock_dir_path
;;

let get_solver_env_from_context ~lock_dir_path =
  let open Fiber.O in
  let+ lock_dir = get_lock_dir_from_context ~lock_dir_path in
  Option.bind lock_dir ~f:(fun lock_dir -> lock_dir.solver_env)
;;

let get_unset_solver_vars_from_context ~lock_dir_path =
  let open Fiber.O in
  let+ lock_dir = get_lock_dir_from_context ~lock_dir_path in
  Option.bind lock_dir ~f:(fun lock_dir -> lock_dir.unset_solver_vars)
;;

let solver_env_from_system_and_context ~lock_dir_path =
  let open Fiber.O in
  let+ solver_env_from_current_system =
    poll_solver_env_from_current_system () >>| Option.some
  and+ solver_env_from_context = get_solver_env_from_context ~lock_dir_path
  and+ unset_solver_vars_from_context =
    get_unset_solver_vars_from_context ~lock_dir_path
  in
  solver_env
    ~solver_env_from_current_system
    ~solver_env_from_context
    ~unset_solver_vars_from_context
;;

module Version_preference = struct
  include Dune_pkg.Version_preference

  let term =
    let all_strings = List.map all_by_string ~f:fst in
    let doc =
      sprintf
        "Whether to prefer the newest compatible version of a package or the oldest \
         compatible version of packages while solving dependencies. This overrides any \
         setting in the current workspace. The default is %s."
        (to_string default)
    in
    let docv = String.concat ~sep:"|" all_strings |> sprintf "(%s)" in
    Arg.(
      value
      & opt (some (enum all_by_string)) None
      & info [ "version-preference" ] ~doc:(Some doc) ~docv)
  ;;

  let choose ~from_arg ~from_context =
    match from_arg, from_context with
    | Some from_arg, _ -> from_arg
    | None, Some from_context -> from_context
    | None, None -> default
  ;;
end

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

let repositories_of_lock_dir workspace ~lock_dir_path =
  match Workspace.find_lock_dir workspace lock_dir_path with
  | Some lock_dir -> lock_dir.repositories
  | None ->
    List.map workspace.repos ~f:(fun repo ->
      let name = Dune_pkg.Pkg_workspace.Repository.name repo in
      let loc = Loc.none in
      loc, name)
;;

let unset_solver_vars_of_workspace workspace ~lock_dir_path =
  let open Option.O in
  let* lock_dir = Workspace.find_lock_dir workspace lock_dir_path in
  lock_dir.unset_solver_vars
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

let find_local_packages =
  let open Memo.O in
  Dune_rules.Dune_load.packages ()
  >>| Package.Name.Map.map ~f:Dune_pkg.Local_package.of_package
;;

let pp_package { Lock_dir.Pkg.info = { Lock_dir.Pkg_info.name; version; avoid; _ }; _ } =
  let warn =
    if avoid
    then Pp.tag User_message.Style.Warning (Pp.text " (this version should be avoided)")
    else Pp.nop
  in
  let open Pp.O in
  Pp.verbatim
    (Package_name.to_string name ^ "." ^ Dune_pkg.Package_version.to_string version)
  ++ warn
;;

let pp_packages packages = Pp.enumerate packages ~f:pp_package

module Lock_dirs_arg = struct
  type t =
    | All
    | Selected of Path.Source.t list

  let all = All

  let term =
    Common.one_of
      (let+ arg =
         Arg.(
           value
           & pos_all string []
           & info
               []
               ~docv:"LOCKDIRS"
               ~doc:
                 (Some
                    "Lock directories to check for outdated packages. Defaults to \
                     dune.lock."))
       in
       Selected (List.map arg ~f:Path.Source.of_string))
      (let+ _all =
         Arg.(
           value
           & flag
           & info
               [ "all" ]
               ~doc:
                 (Some
                    "Check all lock directories in the workspace for outdated packages."))
       in
       All)
  ;;

  let lock_dirs_of_workspace t (workspace : Workspace.t) =
    let module Set = Path.Source.Set in
    let default_path = Dune_rules.Lock_dir.default_source_path in
    let workspace_lock_dirs =
      default_path
      :: List.map workspace.lock_dirs ~f:(fun (lock_dir : Workspace.Lock_dir.t) ->
        lock_dir.path)
      |> Set.of_list
      |> Set.to_list
    in
    match t with
    | All -> workspace_lock_dirs
    | Selected [] -> [ default_path ]
    | Selected chosen_lock_dirs ->
      let workspace_lock_dirs_set = Set.of_list workspace_lock_dirs in
      let chosen_lock_dirs_set = Set.of_list chosen_lock_dirs in
      if Set.is_subset chosen_lock_dirs_set ~of_:workspace_lock_dirs_set
      then chosen_lock_dirs
      else (
        let unknown_lock_dirs =
          Set.diff chosen_lock_dirs_set workspace_lock_dirs_set |> Set.to_list
        in
        let f x = Path.pp (Path.source x) in
        User_error.raise
          [ Pp.text
              "The following directories are not lock directories in this workspace:"
          ; Pp.enumerate unknown_lock_dirs ~f
          ; Pp.text "This workspace contains the following lock directories:"
          ; Pp.enumerate workspace_lock_dirs ~f
          ])
  ;;
end

let check_pkg_management_enabled () =
  Memo.run
  @@
  let open Memo.O in
  let+ workspace = Workspace.workspace () in
  match workspace.config.pkg_enabled with
  | Set (_, `Enabled) | Unset -> ()
  | Set (loc, `Disabled) ->
    User_error.raise
      ~loc
      [ Pp.text "Package management is disabled in workspace configuration." ]
      ~hints:
        [ Pp.text
            "To enable package management, remove the explicit (pkg disabled) setting \
             from your dune-workspace file."
        ]
;;
