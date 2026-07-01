open Import

(* This module is separate from [Action_runner] only because this command
   currently takes too many arguments through [Common]. Once it has its own
   proper argument specification, the wrapper can move back there. *)
let start =
  let+ builder = Common.Builder.term_no_trace_no_pkg
  and+ name =
    Arg.(required & pos 0 (some string) None (Arg.info [] ~docv:"NAME" ~doc:None))
  and+ where =
    Arg.(required & pos 1 (some string) None (Arg.info [] ~docv:"WHERE" ~doc:None))
  and+ trace_fd =
    Arg.(value & opt (some string) None (Arg.info [ "trace-fd" ] ~docv:"FD" ~doc:None))
  in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler_setup.go_without_rpc_server ~common ~config (fun () ->
    Action_runner.start_worker ~name ~where ~trace_fd)
;;

let start = Cmd.v (Cmd.info "start") start
let group = Cmd.group (Cmd.info "action-runner") [ start ]
