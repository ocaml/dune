open Import

let doc = "Moved to dune describe external-lib-deps."

let man =
  [ `S "DESCRIPTION"
  ; `P
      "This subcommand used to print out an approximate set of external libraries that \
       were required for building a given set of targets, without running the build. \
       While this feature was useful, over time the quality of approximation had \
       degraded and the cost of maintenance had increased, so we decided to remove it.\n"
  ; `Blocks Common.help_secs
  ]
;;

let info = Cmd.info "external-lib-deps" ~doc ~man

let term =
  Term.ret
  @@ let+ _ = Common.Builder.term
     and+ _ = Arg.(value & flag & info [ "missing" ] ~doc:{|unused|})
     and+ _ = Arg.(value & pos_all dep [] & Arg.info [] ~docv:"TARGET")
     and+ _ = Arg.(value & flag & info [ "unstable-by-dir" ] ~doc:{|unused|})
     and+ _ = Arg.(value & flag & info [ "sexp" ] ~doc:{|unused|}) in
     `Error (false, "This subcommand has been moved to dune describe external-lib-deps.")
;;

let command = Cmd.v info term
