open Import

let get_local_state () =
  let open Memo.O in
  let+ workspace = Workspace.workspace () in
  Workspace.pkg_enabled workspace
;;

let term =
  let+ builder = Common.Builder.term in
  let client_builder =
    builder |> Common.Builder.forbid_builds |> Common.Builder.disable_log_file
  in
  let _, config = Common.init client_builder in
  Scheduler_setup.no_build_no_rpc ~config (fun () ->
    let open Fiber.O in
    let+ enabled =
      match Global_lock.lock () with
      | Ok () -> Memo.run (get_local_state ())
      | Error lock_held_by ->
        Rpc.Rpc_common.fire_request
          ~name:"pkg-enabled"
          ~wait:false
          ~lock_held_by
          builder
          Dune_rpc_impl.Decl.pkg_enabled
          ()
    in
    match enabled with
    | true -> ()
    | false -> exit 1)
;;

let info =
  let doc =
    "Check if dune's package management features are enabled. If an RPC server is \
     running, use its configuration. Otherwise, use the project configuration and lock \
     directories. Exits with 0 if package management is enabled and 1 otherwise."
  in
  Cmd.info "enabled" ~doc
;;

let command = Cmd.v info term
