open Import

let start =
  let+ builder = Common.Builder.term_no_trace_no_pkg
  and+ name =
    Arg.(required & pos 0 (some string) None (Arg.info [] ~docv:"NAME" ~doc:None))
  and+ socket =
    Arg.(required & pos 1 (some string) None (Arg.info [] ~docv:"SOCKET" ~doc:None))
  in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  let name = Dune_engine.Action_runner.Name.parse_string_exn (Loc.none, name) in
  let where = (`Unix socket : Dune_rpc.Where.t) in
  Scheduler_setup.go_without_rpc_server ~common ~config (fun () ->
    Dune_engine.Action_runner.Worker.start ~name ~where)
;;

let start = Cmd.v (Cmd.info "start") start
let group = Cmd.group (Cmd.info "action-runner") [ start ]
