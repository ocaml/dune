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
      & info [ "version-preference" ] ~doc ~docv)
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

let repositories_of_lock_dir workspace ~lock_dir_path =
  match Workspace.find_lock_dir workspace lock_dir_path with
  | Some lock_dir -> lock_dir.repositories
  | None ->
    List.map Workspace.default_repositories ~f:(fun repo ->
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
      (match Dune_pkg.OpamUrl.local_or_git_only opam_url loc with
       | `Git -> Opam_repo.of_git_repo loc opam_url
       | `Path path -> Fiber.return @@ Opam_repo.of_opam_repo_dir_path loc path))
;;

let find_local_packages =
  let open Memo.O in
  Dune_rules.Dune_load.packages ()
  >>| Package.Name.Map.map ~f:Dune_pkg.Local_package.of_package
;;

let pp_packages packages =
  Pp.enumerate
    packages
    ~f:(fun { Lock_dir.Pkg.info = { Lock_dir.Pkg_info.name; version; _ }; _ } ->
      Pp.verbatim
        (Package_name.to_string name ^ "." ^ Dune_pkg.Package_version.to_string version))
;;

module Lock_dirs_arg = struct
  type t =
    | All
    | Selected of Path.Source.t list

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
                 "Lock directories to check for outdated packages. Defaults to dune.lock.")
       in
       Selected (List.map arg ~f:Path.Source.of_string))
      (let+ _all =
         Arg.(
           value
           & flag
           & info
               [ "all" ]
               ~doc:"Check all lock directories in the workspace for outdated packages.")
       in
       All)
  ;;

  let lock_dirs_of_workspace t (workspace : Workspace.t) =
    let workspace_lock_dirs =
      Lock_dir.default_path
      :: List.map workspace.lock_dirs ~f:(fun (lock_dir : Workspace.Lock_dir.t) ->
        lock_dir.path)
      |> Path.Source.Set.of_list
      |> Path.Source.Set.to_list
    in
    match t with
    | All -> workspace_lock_dirs
    | Selected [] -> [ Lock_dir.default_path ]
    | Selected chosen_lock_dirs ->
      let workspace_lock_dirs_set = Path.Source.Set.of_list workspace_lock_dirs in
      let chosen_lock_dirs_set = Path.Source.Set.of_list chosen_lock_dirs in
      if Path.Source.Set.is_subset chosen_lock_dirs_set ~of_:workspace_lock_dirs_set
      then chosen_lock_dirs
      else (
        let unknown_lock_dirs =
          Path.Source.Set.diff chosen_lock_dirs_set workspace_lock_dirs_set
          |> Path.Source.Set.to_list
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
