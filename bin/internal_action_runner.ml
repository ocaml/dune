open Import

let action_runner_name =
  let parser s =
    match Action_runner_name.parse_string_exn (Loc.none, s) with
    | name -> Ok name
    | exception User_error.E message -> Error (`Msg (User_message.to_string message))
  in
  let printer ppf name = Format.pp_print_string ppf (Action_runner_name.to_string name) in
  Arg.conv (parser, printer)
;;

let fd =
  let parser s =
    match Int.of_string s with
    | Some fd when fd >= 0 -> Ok (Fd.unsafe_of_int fd)
    | Some _ | None -> Error (`Msg (Printf.sprintf "%S is an invalid file descriptor" s))
  in
  let printer ppf fd = Format.pp_print_int ppf (Fd.unsafe_to_int fd) in
  Arg.conv (parser, printer)
;;

(* This module is separate from [Action_runner] only because this command
   currently takes too many arguments through [Common]. Once it has its own
   proper argument specification, the wrapper can move back there. *)
let start =
  let+ builder = Common.Builder.term_no_trace_no_pkg
  and+ name =
    Arg.(
      required & pos 0 (some action_runner_name) None (Arg.info [] ~docv:"NAME" ~doc:None))
  and+ rpc_fd =
    Arg.(required & opt (some fd) None (Arg.info [ "rpc-fd" ] ~docv:"FD" ~doc:None))
  and+ trace_fd =
    Arg.(value & opt (some fd) None (Arg.info [ "trace-fd" ] ~docv:"FD" ~doc:None))
  in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler_setup.go_without_rpc_server ~common ~config (fun () ->
    Action_runner.start_worker ~name ~rpc_fd ~trace_fd)
;;

let start = Cmd.v (Cmd.info "start") start
let group = Cmd.group (Cmd.info "action-runner") [ start ]
