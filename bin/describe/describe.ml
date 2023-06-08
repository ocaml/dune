open Import

let group =
  let doc = "Describe the workspace." in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        {|Describe what is in the current workspace in either human or
        machine readable form.

        By default, this command output a human readable description of
        the current workspace. This output is aimed at human and is not
        suitable for machine processing. In particular, it is not versioned.

        If you want to interpret the output of this command from a program,
        you must use the $(b,--format) option to specify a machine readable
        format as well as the $(b,--lang) option to get a stable output.|}
    ; `Blocks Common.help_secs
    ]
  in
  let info = Cmd.info "describe" ~doc ~man in
  let default = Describe_workspace.term in
  Cmd.group ~default info
    [ Describe_workspace.command
    ; Describe_external_lib_deps.command
    ; Describe_opam_files.command
    ; Describe_pp.command
    ]
