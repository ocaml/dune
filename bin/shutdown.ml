open Import

let info =
  let doc = "Cancel and shutdown any builds in the current workspace." in
  Cmd.info "shutdown" ~doc
;;

let term =
  let+ builder = Common.Builder.term in
  Rpc.Rpc_common.client_term
    builder
    (Rpc.Rpc_common.fire_notification
       ~name:"shutdown_cmd"
       ~wait:false
       builder
       Dune_rpc_private.Procedures.Public.shutdown)
;;

let command = Cmd.v info term
