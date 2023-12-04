open Import

let info = Cmd.info "ocaml" ~doc:"Command group related to OCaml."

let group =
  Cmdliner.Cmd.group
    info
    [ Utop.command
    ; Ocaml_merlin.command
    ; Ocaml_merlin.Dump_dot_merlin.command
    ; Top.command
    ; Top.module_command
    ; Ocaml_merlin.group
    ; Doc.cmd
    ]
;;
