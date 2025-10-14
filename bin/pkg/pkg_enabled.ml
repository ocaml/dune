open Import

let term =
  let+ builder = Common.Builder.term in
  let common, config = Common.init builder in
  Scheduler.go_with_rpc_server ~common ~config (fun () ->
    Memo.run
    @@
    let open Memo.O in
    let+ workspace = Workspace.workspace () in
    let lock_dir_paths =
      Pkg_common.Lock_dirs_arg.lock_dirs_of_workspace
        Pkg_common.Lock_dirs_arg.all
        workspace
    in
    let any_lockdir_exists =
      List.exists lock_dir_paths ~f:(fun p -> Path.exists (Path.source p))
    in
    (* CR-Leonidas-from-XIV: change this logic when we stop detecting lock
       directories in the source tree *)
    let enabled =
      match workspace.config.pkg_enabled with
      | Set (_, `Enabled) -> true
      | Set (_, `Disabled) -> false
      | Unset -> any_lockdir_exists
    in
    match enabled with
    | true -> ()
    | false -> exit 1)
;;

let info =
  let doc =
    "Check if the project indicates that dune's package management features should be \
     enabled. Exits with 0 if package management is enabled and 1 otherwise."
  in
  Cmd.info "enabled" ~doc
;;

let command = Cmd.v info term
