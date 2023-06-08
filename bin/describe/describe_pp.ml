open Import

let term =
  let+ common = Common.term
  and+ context_name = Common.context_arg ~doc:"Build context to use."
  and+ file =
    Arg.(required & pos 0 (some string) None (Arg.info [] ~docv:"FILE"))
  in
  let config = Common.init common in
  Scheduler.go ~common ~config @@ fun () ->
  let open Fiber.O in
  let* setup = Import.Main.setup () in
  let* setup = Memo.run setup in
  let super_context = Import.Main.find_scontext_exn setup ~name:context_name in
  Build_system.run_exn (fun () ->
      Describe_common.Preprocess.run super_context file)

let command =
  let doc = "builds a given FILE and prints the preprocessed output" in
  let info = Cmd.info ~doc "pp" in
  Cmd.v info term
