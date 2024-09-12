open Import
open Pkg_common
module Package_version = Dune_pkg.Package_version
module Opam_repo = Dune_pkg.Opam_repo
module Lock_dir = Dune_pkg.Lock_dir
module Pin_stanza = Dune_pkg.Pin_stanza

module Progress_indicator = struct
  module Per_lockdir = struct
    module State = struct
      module Repository = Dune_pkg.Pkg_workspace.Repository

      type t =
        | Updating_repos of Repository.Name.t list
        | Solving

      let pp = function
        | Updating_repos repo_names ->
          Pp.textf
            "Updating package repos %s..."
            (List.map repo_names ~f:(fun repo_name ->
               Repository.Name.to_string repo_name |> String.quoted)
             |> String.enumerate_and)
        | Solving -> Pp.text "Solving..."
      ;;
    end

    type t =
      { lockdir_path : Path.Source.t
      ; state : State.t option ref
      }

    let create lockdir_path = { lockdir_path; state = ref None }
  end

  (* The progress indicator for the entire lock operation, which may
     involve generating multiple lockdirs *)
  type t = Per_lockdir.t list

  let pp (t : t) =
    (* Only display the first non-done lockdir state, since the status
       line can only consist of a single line. *)
    List.find_map t ~f:(fun { Per_lockdir.lockdir_path; state } ->
      Option.map !state ~f:(fun state ->
        Pp.concat
          [ Pp.textf "Locking %s: " (Path.Source.to_string_maybe_quoted lockdir_path)
          ; Per_lockdir.State.pp state
          ]))
    |> Option.value ~default:Pp.nop
  ;;

  let add_overlay (t : t) = Console.Status_line.add_overlay (Live (fun () -> pp t))
end

let resolve_project_sources sources =
  let scan_project ~read ~files =
    let read file = Memo.of_reproducible_fiber (read file) in
    let open Memo.O in
    Dune_project.gen_load ~read ~files ~dir:Path.Source.root ~infer_from_opam_files:false
    >>| Option.map ~f:(fun project ->
      let sources = Dune_project.sources project in
      let packages = Dune_project.packages project in
      sources, packages)
    |> Memo.run
  in
  Pin_stanza.resolve sources ~scan_project
;;

let solve_lock_dir
  workspace
  ~local_packages
  ~project_sources
  version_preference
  solver_env_from_current_system
  lock_dir_path
  progress_state
  =
  let open Fiber.O in
  let lock_dir = Workspace.find_lock_dir workspace lock_dir_path in
  let project_sources =
    match lock_dir with
    | None -> project_sources
    | Some lock_dir ->
      let workspace =
        Pin_stanza.DB.Workspace.extract workspace.sources ~names:lock_dir.pins
      in
      Pin_stanza.DB.combine_exn workspace project_sources
  in
  let solver_env =
    solver_env
      ~solver_env_from_context:
        (Option.bind lock_dir ~f:(fun lock_dir -> lock_dir.solver_env))
      ~solver_env_from_current_system
      ~unset_solver_vars_from_context:
        (unset_solver_vars_of_workspace workspace ~lock_dir_path)
  in
  let* repos =
    let repo_map = repositories_of_workspace workspace in
    let repo_names =
      Dune_pkg.Pkg_workspace.Repository.Name.Map.keys repo_map
      |> List.sort ~compare:Dune_pkg.Pkg_workspace.Repository.Name.compare
    in
    progress_state
    := Some (Progress_indicator.Per_lockdir.State.Updating_repos repo_names);
    get_repos repo_map ~repositories:(repositories_of_lock_dir workspace ~lock_dir_path)
  in
  let* pins = resolve_project_sources project_sources in
  progress_state := Some Progress_indicator.Per_lockdir.State.Solving;
  Dune_pkg.Opam_solver.solve_lock_dir
    solver_env
    (Pkg_common.Version_preference.choose
       ~from_arg:version_preference
       ~from_context:
         (Option.bind lock_dir ~f:(fun lock_dir -> lock_dir.version_preference)))
    repos
    ~pins
    ~local_packages:
      (Package_name.Map.map local_packages ~f:Dune_pkg.Local_package.for_solver)
    ~constraints:(constraints_of_workspace workspace ~lock_dir_path)
  >>= function
  | Error (`Diagnostic_message message) -> Fiber.return (Error (lock_dir_path, message))
  | Ok { lock_dir; files; pinned_packages } ->
    let summary_message =
      User_message.make
        [ Pp.tag
            User_message.Style.Success
            (Pp.textf
               "Solution for %s:"
               (Path.Source.to_string_maybe_quoted lock_dir_path))
        ; (match Package_name.Map.values lock_dir.packages with
           | [] ->
             Pp.tag User_message.Style.Warning @@ Pp.text "(no dependencies to lock)"
           | packages -> pp_packages packages)
        ]
    in
    progress_state := None;
    let+ lock_dir = Lock_dir.compute_missing_checksums ~pinned_packages lock_dir in
    Ok (Lock_dir.Write_disk.prepare ~lock_dir_path ~files lock_dir, summary_message)
;;

let solve
  workspace
  ~local_packages
  ~project_sources
  ~solver_env_from_current_system
  ~version_preference
  ~lock_dirs
  =
  let open Fiber.O in
  (* a list of thunks that will perform all the file IO side
     effects after performing validation so that if materializing any
     lockdir would fail then no side effect takes place. *)
  (let+ errors, solutions =
     let progress_indicator =
       List.map lock_dirs ~f:Progress_indicator.Per_lockdir.create
     in
     let overlay = Progress_indicator.add_overlay progress_indicator in
     let+ result =
       Fiber.finalize
         ~finally:(fun () ->
           Console.Status_line.remove_overlay overlay;
           Fiber.return ())
         (fun () ->
           Fiber.parallel_map progress_indicator ~f:(fun { lockdir_path; state } ->
             solve_lock_dir
               workspace
               ~local_packages
               ~project_sources
               version_preference
               solver_env_from_current_system
               lockdir_path
               state))
     in
     List.partition_map result ~f:Result.to_either
   in
   match errors with
   | [] -> Ok solutions
   | _ -> Error errors)
  >>| function
  | Error errors ->
    User_error.raise
      ([ Pp.text "Unable to solve dependencies for the following lock directories:" ]
       @ List.concat_map errors ~f:(fun (path, message) ->
         [ Pp.textf "Lock directory %s:" (Path.Source.to_string_maybe_quoted path)
         ; Pp.hovbox message
         ]))
  | Ok write_disks_with_summaries ->
    let write_disk_list, summary_messages = List.split write_disks_with_summaries in
    List.iter summary_messages ~f:Console.print_user_message;
    (* All the file IO side effects happen here: *)
    List.iter write_disk_list ~f:Lock_dir.Write_disk.commit
;;

let project_sources =
  let open Memo.O in
  Dune_rules.Dune_load.projects ()
  >>| List.fold_left ~init:Pin_stanza.DB.empty ~f:(fun acc project ->
    Pin_stanza.DB.combine_exn acc (Dune_project.sources project))
;;

let lock ~version_preference ~lock_dirs_arg =
  let open Fiber.O in
  let* solver_env_from_current_system =
    Dune_pkg.Sys_poll.make ~path:(Env_path.path Stdune.Env.initial)
    |> Dune_pkg.Sys_poll.solver_env_from_current_system
    >>| Option.some
  and* workspace, local_packages, project_sources =
    Memo.run
    @@
    let open Memo.O in
    let+ workspace = Workspace.workspace ()
    and+ local_packages = find_local_packages
    and+ project_sources = project_sources in
    workspace, local_packages, project_sources
  in
  let lock_dirs =
    Pkg_common.Lock_dirs_arg.lock_dirs_of_workspace lock_dirs_arg workspace
  in
  solve
    workspace
    ~local_packages
    ~project_sources
    ~solver_env_from_current_system
    ~version_preference
    ~lock_dirs
;;

let term =
  let+ builder = Common.Builder.term
  and+ version_preference = Version_preference.term
  and+ lock_dirs_arg = Pkg_common.Lock_dirs_arg.term in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go ~common ~config (fun () -> lock ~version_preference ~lock_dirs_arg)
;;

let info =
  let doc = "Create a lockfile" in
  Cmd.info "lock" ~doc
;;

let command = Cmd.v info term
