open Import

let term =
  let+ builder = Common.Builder.term in
  let common, config = Common.init builder in
  Scheduler_setup.go_with_rpc_server ~common ~config (fun () ->
    Memo.run
    @@
    let open Memo.O in
    let* workspace = Workspace.workspace () in
    (* CR-Leonidas-from-XIV: change this logic when we stop detecting lock
       directories in the source tree *)
    let+ enabled =
      match workspace.config.pkg_enabled with
      | Set (_, `Enabled) -> Memo.return true
      | Set (_, `Disabled) -> Memo.return false
      | Unset ->
        let lock_dir_paths =
          Pkg_common.Lock_dirs_arg.lock_dirs_of_workspace
            Pkg_common.Lock_dirs_arg.all
            workspace
        in
        let+ active =
          Memo.List.exists workspace.contexts ~f:(fun ctx ->
            Dune_rules.Lock_dir.lock_dir_active (Workspace.Context.name ctx))
        in
        active && List.exists lock_dir_paths ~f:(fun p -> Path.exists (Path.source p))
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
