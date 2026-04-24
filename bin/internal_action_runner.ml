open Import

let start =
  let+ builder = Common.Builder.term_no_trace_no_pkg
  and+ name =
    Arg.(required & pos 0 (some string) None (Arg.info [] ~docv:"NAME" ~doc:None))
  and+ where =
    Arg.(required & pos 1 (some string) None (Arg.info [] ~docv:"WHERE" ~doc:None))
  and+ trace_fd =
    Arg.(value & opt (some int) None (Arg.info [ "trace-fd" ] ~docv:"FD" ~doc:None))
  in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  let name = Dune_engine.Action_runner.Name.parse_string_exn (Loc.none, name) in
  Option.iter trace_fd ~f:(fun fd ->
    Dune_trace.set_global_inherited_fd (Fd.unsafe_of_int fd);
    Dune_trace.set_common_args
      [ "action_runner", Sexp.Atom (Dune_engine.Action_runner.Name.to_string name) ]);
  let where =
    match
      Dune_rpc.Conv.of_sexp
        Dune_rpc.Where.sexp
        ~version:Dune_rpc.Version.latest
        (Sexp.Atom where)
    with
    | Ok where -> where
    | Error err ->
      User_error.raise
        [ Pp.textf "invalid action runner RPC address %S" where
        ; Pp.text (Dyn.to_string (Dune_rpc.Conv.dyn_of_error err))
        ]
  in
  Scheduler_setup.go_without_rpc_server ~common ~config (fun () ->
    Dune_engine.Action_runner.Worker.start ~name ~where)
;;

let start = Cmd.v (Cmd.info "start") start
let group = Cmd.group (Cmd.info "action-runner") [ start ]
