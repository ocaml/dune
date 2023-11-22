open Import
module Lock_dir = Dune_pkg.Lock_dir

let context_term ~doc =
  Arg.(value & opt (some Arg.context_name) None & info [ "context" ] ~docv:"CONTEXT" ~doc)
;;

(* The system environment variables used by the solver are taken from the
   current system by default but can be overridden by the build context. *)
let solver_env_variables ~solver_sys_vars_from_context ~sys_bindings_from_current_system =
  match solver_sys_vars_from_context with
  | None -> sys_bindings_from_current_system
  | Some solver_env_variables ->
    Dune_pkg.Solver_env.Variable.Sys.Bindings.extend
      sys_bindings_from_current_system
      solver_env_variables
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

module Per_context = struct
  type t =
    { lock_dir_path : Path.Source.t
    ; version_preference : Version_preference.t
    ; solver_sys_vars : Dune_pkg.Solver_env.Variable.Sys.Bindings.t option
    ; repositories : Dune_pkg.Pkg_workspace.Repository.Name.t list
    ; context_common : Dune_rules.Workspace.Context.Common.t
    ; repos :
        Dune_pkg.Pkg_workspace.Repository.t Dune_pkg.Pkg_workspace.Repository.Name.Map.t
    }

  let repositories_of_workspace (workspace : Workspace.t) =
    List.map workspace.repos ~f:(fun repo ->
      Dune_pkg.Pkg_workspace.Repository.name repo, repo)
    |> Dune_pkg.Pkg_workspace.Repository.Name.Map.of_list_exn
  ;;

  let choose ~context_name_arg ~all_contexts_arg ~version_preference_arg =
    let open Fiber.O in
    match context_name_arg, all_contexts_arg with
    | Some _, true ->
      User_error.raise [ Pp.text "--context and --all-contexts are mutually exclusive" ]
    | context_name_opt, false ->
      let+ workspace = Memo.run (Workspace.workspace ()) in
      let context_name =
        Option.value context_name_opt ~default:Dune_engine.Context_name.default
      in
      let context =
        (* TODO this doesn't work for target contexts defined by cross compilation *)
        List.find workspace.contexts ~f:(fun context ->
          Dune_engine.Context_name.equal (Workspace.Context.name context) context_name)
      in
      (match context with
       | None ->
         User_error.raise
           [ Pp.textf
               "Unknown build context: %s"
               (Dune_engine.Context_name.to_string context_name |> String.maybe_quoted)
           ]
       | Some
           (Default
             { lock
             ; version_preference = version_preference_context
             ; solver_sys_vars
             ; repositories
             ; base = context_common
             ; _
             }) ->
         [ { lock_dir_path = Option.value lock ~default:Lock_dir.default_path
           ; version_preference =
               Version_preference.choose
                 ~from_arg:version_preference_arg
                 ~from_context:version_preference_context
           ; solver_sys_vars
           ; repositories
           ; context_common
           ; repos = repositories_of_workspace workspace
           }
         ]
       | Some (Opam _) ->
         User_error.raise
           [ Pp.textf
               "Unexpected opam build context: %s"
               (Dune_engine.Context_name.to_string context_name |> String.maybe_quoted)
           ])
    | None, true ->
      let+ workspace = Memo.run (Workspace.workspace ()) in
      List.filter_map workspace.contexts ~f:(function
        | Workspace.Context.Default
            { lock
            ; version_preference = version_preference_context
            ; base = context_common
            ; solver_sys_vars
            ; repositories
            } ->
          let lock_dir_path = Option.value lock ~default:Dune_pkg.Lock_dir.default_path in
          Some
            { lock_dir_path
            ; version_preference =
                Version_preference.choose
                  ~from_arg:version_preference_arg
                  ~from_context:version_preference_context
            ; context_common
            ; solver_sys_vars
            ; repositories
            ; repos = repositories_of_workspace workspace
            }
        | Opam _ -> None)
  ;;
end

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

let get_repos
  repos
  ~opam_repository_path
  ~opam_repository_url
  ~repositories
  ~update_opam_repositories
  =
  let module Repository_id = Dune_pkg.Repository_id in
  let module Opam_repo = Dune_pkg.Opam_repo in
  let open Fiber.O in
  match opam_repository_path, opam_repository_url with
  | Some _, Some _ ->
    (* in theory you can set both, but how to prioritize them? *)
    User_error.raise [ Pp.text "Can't specify both path and URL to an opam-repository" ]
  | Some path, None ->
    let repo_id = Repository_id.of_path path in
    Fiber.return @@ [ Opam_repo.of_opam_repo_dir_path ~source:None ~repo_id path ]
  | None, Some (url : OpamUrl.t) ->
    let+ opam_repo =
      Opam_repo.of_git_repo
        ~repo_id:None
        ~update:update_opam_repositories
        ~source:url.path
    in
    [ opam_repo ]
  | None, None ->
    repositories
    |> Fiber.parallel_map ~f:(fun name ->
      match Dune_pkg.Pkg_workspace.Repository.Name.Map.find repos name with
      | None ->
        (* TODO: have loc for this failure? *)
        User_error.raise
          [ Pp.textf "Repository '%s' is not a known repository"
            @@ Dune_pkg.Pkg_workspace.Repository.Name.to_string name
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

module Opam_repository_path = struct
  let term =
    let dune_path =
      let parser s =
        s
        |> Path.External.of_filename_relative_to_initial_cwd
        |> Path.external_
        |> Result.ok
      in
      let printer pf p = Pp.to_fmt pf (Path.pp p) in
      Arg.conv (parser, printer)
    in
    Arg.(
      value
      & opt (some dune_path) None
      & info
          [ "opam-repository-path" ]
          ~docv:"PATH"
          ~doc:
            "Path to a local opam repository. This should be a directory containing a \
             valid opam repository such as the one at \
             https://github.com/ocaml/opam-repository.")
  ;;
end

module Opam_repository_url = struct
  let term =
    let parser s =
      match OpamUrl.parse_opt s with
      | Some url -> Ok url
      | None -> Error (`Msg "URL can't be parsed")
    in
    let printer pf u = Pp.to_fmt pf (Pp.text (OpamUrl.to_string u)) in
    let opam_url = Arg.conv (parser, printer) in
    Arg.(
      value
      & opt (some opam_url) None
      & info
          [ "opam-repository-url" ]
          ~docv:"URL"
          ~doc:
            "URL of opam repository to download. Can be either a git repository or a \
             link to the tarball of a repository.")
  ;;
end

let pp_packages packages =
  let module Package_version = Dune_pkg.Package_version in
  Pp.enumerate
    packages
    ~f:(fun { Lock_dir.Pkg.info = { Lock_dir.Pkg_info.name; version; _ }; _ } ->
      Pp.verbatim (Package_name.to_string name ^ "." ^ Package_version.to_string version))
;;
