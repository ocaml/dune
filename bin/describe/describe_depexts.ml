open Import

let print_depexts context_name =
  let open Fiber.O in
  let+ depexts =
    build_exn (fun () -> Dune_rules.Pkg_rules.all_filtered_depexts context_name)
  in
  Console.print [ Pp.concat_map ~sep:Pp.newline ~f:Pp.verbatim depexts ]
;;

let term =
  let+ builder = Common.Builder.term
  and+ context_name = Common.context_arg ~doc:"Build context to use." in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go_with_rpc_server ~common ~config (fun () -> print_depexts context_name)
;;

let info =
  let doc = "Print the list of all the available depexts" in
  Cmd.info "depexts" ~doc
;;

let command = Cmd.v info term
