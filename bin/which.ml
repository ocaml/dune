open Import

let doc =
  "Print the path to the executable using the same resolution logic as [dune exec]."
;;

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune which NAME) prints the path to the executable NAME using the same logic as:
          |}
  ; `Pre "$ dune exec NAME"
  ; `P
      "Dune will first try to resolve the executable within the public executables in \
       the current project, then inside the \"bin\" directory of each package among the \
       project's  dependencies (when using dune package management), and finally within \
       the  directories listed in the $PATH environment variable."
  ]
;;

let info = Cmd.info "which" ~doc ~man

let term : unit Term.t =
  let+ builder = Common.Builder.term
  and+ ctx_name = Common.context_arg ~doc:{|Run the command in this build context.|}
  and+ prog = Arg.(required & pos 0 (some string) None (Arg.info [] ~docv:"PROG")) in
  let common, config = Common.init builder in
  Scheduler.go_with_rpc_server ~common ~config
  @@ fun () ->
  build_exn
  @@ fun () ->
  let open Memo.O in
  let+ path = Exec.get_path common ctx_name ~prog >>| Path.to_string in
  Console.print [ Pp.verbatim path ]
;;

let command = Cmd.v info term
