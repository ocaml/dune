open Import

let info =
  let doc = "Experimental package management" in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Commands for doing package management with dune|}
    ; `Blocks Common.help_secs
    ]
  in
  Cmd.info "pkg" ~doc ~man
;;

let group =
  Cmd.group
    info
    [ Lock.command
    ; Print_solver_env.command
    ; Outdated.command
    ; Validate_lock_dir.command
    ]
;;
