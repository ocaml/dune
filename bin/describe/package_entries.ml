open Import

let term =
  let+ builder = Common.Builder.term
  and+ context_name = Common.context_arg ~doc:(Some "Build context to use.")
  and+ format = Describe_format.arg in
  Build.describe builder ~context_name (fun _common _setup super_context ->
    let open Memo.O in
    Dune_rules.Install_rules.stanzas_to_entries super_context
    >>| Package.Name.Map.to_dyn (Dyn.list Install.Entry.Sourced.Unexpanded.to_dyn)
    >>| Describe_format.print_dyn format)
;;

let command =
  let doc = "prints information about the entries per package." in
  let info = Cmd.info ~doc "package-entries" in
  Cmd.v info term
;;
