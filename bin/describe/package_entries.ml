open Import

let term =
  let+ builder = Common.Builder.term
  and+ context_name = Common.context_arg ~doc:"Build context to use."
  and+ format = Describe_format.arg in
  let common, config = Common.init builder in
  Scheduler.go ~common ~config
  @@ fun () ->
  let open Fiber.O in
  let* setup = Import.Main.setup () in
  let* setup = Memo.run setup in
  let super_context = Import.Main.find_scontext_exn setup ~name:context_name in
  Build_system.run_exn
  @@ fun () ->
  let open Memo.O in
  Dune_rules.Install_rules.stanzas_to_entries super_context
  >>| Package.Name.Map.to_dyn (Dyn.list Install.Entry.Sourced.to_dyn)
  >>| Describe_format.print_dyn format
;;

let command =
  let doc = "prints information about the entries per package." in
  let info = Cmd.info ~doc "package-entries" in
  Cmd.v info term
;;
