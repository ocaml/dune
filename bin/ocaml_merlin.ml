open! Stdune
open Import

let doc = "Start a merlin configuration server"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune ocaml-merlin) starts a server that can be queried to get
      .merlin information. It is meant to be used by Merlin itself and does not
      provide a user-friendly output.|}
  ; `Blocks Common.help_secs
  ; Common.footer
  ]

let info = Term.info "ocaml-merlin" ~doc ~man

let term =
  let+ common = Common.term
  and+ dump_config =
    Arg.(
      value
      & opt ~vopt:(Some ".") (some string) None
      & info [ "dump-config" ]
          ~doc:
            "Prints the entire content of the merlin configuration for the \
             given folder in a user friendly form. This is for testing and \
             debugging purposes only and should not be considered as a stable \
             output.")
  in
  let common = Common.set_print_directory common false in
  let config = Common.init common ~log_file:No_log_file in
  Scheduler.go ~common ~config (fun () ->
      match dump_config with
      | Some s -> Dune_rules.Merlin_server.dump s
      | None -> Dune_rules.Merlin_server.start ())

let command = (term, info)

module Dump_dot_merlin = struct
  let doc = "Print Merlin configuration"

  let man =
    [ `S "DESCRIPTION"
    ; `P
        {|$(b,dune ocaml dump-dot-merlin) will attempt to read previously
        generated configuration in a source folder, merge them and print
        it to the standard output in Merlin configuration syntax. The
        output of this command should always be checked and adapted to
        the project needs afterward.|}
    ; Common.footer
    ]

  let info = Term.info "dump-dot-merlin" ~doc ~man

  let term =
    let+ common = Common.term
    and+ path =
      Arg.(
        value
        & pos 0 (some string) None
        & info [] ~docv:"PATH"
            ~doc:
              "The path to the folder of which the configuration should be \
               printed. Defaults to the current directory.")
    in
    let config = Common.init common ~log_file:No_log_file in
    Scheduler.go ~common ~config (fun () ->
        match path with
        | Some s -> Dune_rules.Merlin_server.dump_dot_merlin s
        | None -> Dune_rules.Merlin_server.dump_dot_merlin ".")

  let command = (term, info)
end
