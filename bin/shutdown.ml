open Import

let info =
  let doc = "Cancel and shutdown any builds in the current workspace." in
  Cmd.info "shutdown" ~doc
;;

let term =
  let+ builder = Common.Builder.term in
  Rpc.Common.client_term
    builder
    (Rpc.Common.fire_message
       ~name:"shutdown_cmd"
       ~wait:false
       builder
       (Rpc.Common.Notification Dune_rpc_private.Procedures.Public.shutdown))
;;

let command = Cmd.v info term
