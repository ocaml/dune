open! Stdune
open Import

let cmd = Dune_cmd.cmd

let exit_and_flush code =
  Console.finish ();
  exit code

let () =
  Dune_rules.Colors.setup_err_formatter_colors ();
  try
    match Cmd.eval_value cmd ~catch:false with
    | Ok _ -> exit_and_flush 0
    | Error _ -> exit_and_flush 1
  with
  | Scheduler.Run.Shutdown.E Requested -> exit_and_flush 0
  | Scheduler.Run.Shutdown.E (Signal _) -> exit_and_flush 130
  | exn ->
    let exn = Exn_with_backtrace.capture exn in
    Dune_util.Report_error.report exn;
    exit_and_flush 1
