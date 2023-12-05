open Import
module Lock_dir = Dune_pkg.Lock_dir
module Solver_env = Dune_pkg.Solver_env
module Variable_name = Dune_pkg.Variable_name
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
  let lock_dir = Workspace.find_lock_dir workspace lock_dir_path in
  match lock_dir with
  | None -> []
  | Some lock_dir -> lock_dir.constraints
;;

let repositories_of_lock_dir workspace ~lock_dir_path =
  let lock_dir = Workspace.find_lock_dir workspace lock_dir_path in
  Option.map lock_dir ~f:(fun lock_dir -> lock_dir.repositories)
  |> Option.value
       ~default:
         (List.map
            Workspace.default_repositories
            ~f:Dune_pkg.Pkg_workspace.Repository.name)
;;

let unset_solver_vars_of_workspace workspace ~lock_dir_path =
  let open Option.O in
  let* lock_dir = Workspace.find_lock_dir workspace lock_dir_path in
  lock_dir.unset_solver_vars
;;

let location_of_opam_url url =
  match (url : OpamUrl.t).backend with
  | `rsync -> `Path (Path.of_string url.path)
  (* contrary to OPAM we also attempt to load HTTP sources via git *)
  | `git | `http -> `Git (OpamUrl.base_url url)
  | `darcs | `hg ->
    User_error.raise
      ~hints:[ Pp.text "Specify either a file path or git repo via SSH/HTTPS" ]
      [ Pp.textf "Could not determine location of repository %s" @@ OpamUrl.to_string url
      ]
;;

let get_repos repos ~repositories ~update_opam_repositories =
  let module Repository_id = Dune_pkg.Repository_id in
  let module Opam_repo = Dune_pkg.Opam_repo in
  let module Repository = Dune_pkg.Pkg_workspace.Repository in
  repositories
  |> Fiber.parallel_map ~f:(fun name ->
    match Repository.Name.Map.find repos name with
    | None ->
      (* TODO: have loc for this failure? *)
      User_error.raise
        [ Pp.textf "Repository '%s' is not a known repository"
          @@ Repository.Name.to_string name
        ]
    | Some repo ->
      let opam_url = Dune_pkg.Pkg_workspace.Repository.opam_url repo in
      (match location_of_opam_url opam_url with
       | `Git source ->
         Opam_repo.of_git_repo ~repo_id:None ~update:update_opam_repositories ~source
       | `Path path ->
         let repo_id = Repository_id.of_path path in
         Fiber.return @@ Opam_repo.of_opam_repo_dir_path ~source:None ~repo_id path))
;;

let find_local_packages =
  let open Fiber.O in
  let+ project =
    let+ source_dir = Memo.run (Source_tree.root ()) in
    Source_tree.Dir.project source_dir
  in
  Dune_project.packages project |> Package.Name.Map.map ~f:Package.to_local_package
;;

let pp_packages packages =
  let module Package_version = Dune_pkg.Package_version in
  Pp.enumerate
    packages
    ~f:(fun { Lock_dir.Pkg.info = { Lock_dir.Pkg_info.name; version; _ }; _ } ->
      Pp.verbatim (Package_name.to_string name ^ "." ^ Package_version.to_string version))
;;

module Lock_dirs = struct
  let term =
    let+ arg =
      Arg.(
        value
        & pos_all string []
        & info
            []
            ~docv:"LOCKDIRS"
            ~doc:"Lock directories to check for outdated packages. Defaults to dune.lock.")
    in
    List.map arg ~f:Path.Source.of_string
  ;;

  let of_workspace (workspace : Workspace.t) ~chosen_lock_dirs =
    let workspace_lock_dirs =
      List.filter_map workspace.contexts ~f:(function
        | Workspace.Context.Default { lock_dir; base = _ } ->
          let lock_dir_path =
            Option.value lock_dir ~default:Dune_pkg.Lock_dir.default_path
          in
          Some lock_dir_path
        | Opam _ -> None)
    in
    match chosen_lock_dirs with
    | [] -> workspace_lock_dirs
    | _ ->
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
