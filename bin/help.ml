open Stdune
open Import

let config =
  ( ("dune-config", 5, "", "Dune", "Dune manual")
  , [ `S Manpage.s_name
    ; `P {|dune-config - configuring the dune build system|}
    ; `S Manpage.s_synopsis
    ; `Pre "~/.config/dune/config"
    ; `S Manpage.s_description
    ; `P
        {|Unless $(b,--no-config) or $(b,-p) is passed, Dune will read a
           configuration file from the user home directory. This file is used
           to control various aspects of the behavior of Dune.|}
    ; `P
        {|The configuration file is normally $(b,~/.config/dune/config) on
           Unix systems and $(b,Local Settings/dune/config) in the User home
           directory on Windows. However, it is possible to specify an
           alternative configuration file with the $(b,--config-file) option.|}
    ; `P
        {|The first line of the file must be of the form (lang dune X.Y)
           where X.Y is the version of the dune language used in the file.|}
    ; `P
        {|The rest of the file must be written in S-expression syntax and be
           composed of a list of stanzas. The following sections describe
           the stanzas available.|}
    ; `S "CACHING"
    ; `P {|Syntax: $(b,\(cache ENABLED\))|}
    ; `P
        {| This stanza determines whether dune's build caching is enabled.
           See https://dune.readthedocs.io/en/stable/caching.html for details.
           Valid values for $(b, ENABLED) are $(b, enabled) or $(b, disabled).|}
    ; `S "DISPLAY MODES"
    ; `P {|Syntax: $(b,\(display MODE\))|}
    ; `P
        {|This stanza controls how Dune reports what it is doing to the user.
           This parameter can also be set from the command line via $(b,--display MODE).
           The following display modes are available:|}
    ; `Blocks
        (List.map
           ~f:(fun (x, desc) -> `I (sprintf "$(b,%s)" x, desc))
           [ ( "progress"
             , {|This is the default, Dune shows and update a
               status line as build goals are being completed.|}
             )
           ; ("quiet", {|Only display errors.|})
           ; ( "short"
             , {|Print one line per command being executed, with the
               binary name on the left and the reason it is being executed for
               on the right.|}
             )
           ; ( "verbose"
             , {|Print the full command lines of programs being
               executed by Dune, with some colors to help differentiate
               programs.|}
             )
           ])
    ; `P
        {|Note that when the selected display mode is $(b,progress) and the
           output is not a terminal then the $(b,quiet) mode is selected
           instead. This rule doesn't apply when running Dune inside Emacs.
           Dune detects whether it is executed from inside Emacs or not by
           looking at the environment variable $(b,INSIDE_EMACS) that is set by
           Emacs. If you want the same behavior with another editor, you can set
           this variable. If your editor already sets another variable,
           please open a ticket on the ocaml/dune GitHub project so that we can
           add support for it.|}
    ; `S "JOBS"
    ; `P {|Syntax: $(b,\(jobs NUMBER\))|}
    ; `P
        {|Set the maximum number of jobs Dune might run in parallel.
           This can also be set from the command line via $(b,-j NUMBER).|}
    ; `P {|The default for this value is the number of processors.|}
    ; `S "SANDBOXING"
    ; `P {|Syntax: $(b,\(sandboxing_preference MODE ...\))|}
    ; `P
        {|Controls the sandboxing mode preference order used by dune. Dune will
use the earliest item from this list that's allowed by the action dependency
specification, or fall back on the hard-coded default. See $(b,man dune-build)
 for the description of individual modes.|}
    ; Common.footer
    ] )

type what =
  | Man of Manpage.t
  | List_topics

let commands = [ ("config", Man config); ("topics", List_topics) ]

let doc = "Additional Dune help"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune help TOPIC) provides additional help on the given topic.
            The following topics are available:|}
  ; `Blocks
      (List.concat_map commands ~f:(fun (s, what) ->
           match what with
           | List_topics -> []
           | Man ((title, _, _, _, _), _) -> [ `I (sprintf "$(b,%s)" s, title) ]))
  ; Common.footer
  ]

let info = Cmd.info "help" ~doc ~man

let term =
  Term.ret
  @@ let+ man_format = Arg.man_format
     and+ what =
       Arg.(value & pos 0 (some (enum commands)) None & info [] ~docv:"TOPIC")
     and+ () = Common.build_info in
     match what with
     | None -> `Help (man_format, Some "help")
     | Some (Man man_page) ->
       Format.printf "%a@?" (Manpage.print man_format) man_page;
       `Ok ()
     | Some List_topics ->
       List.filter_map commands ~f:(fun (s, what) ->
           match what with
           | List_topics -> None
           | _ -> Some s)
       |> List.sort ~compare:String.compare
       |> String.concat ~sep:"\n" |> print_endline;
       `Ok ()

let command = Cmd.v info term
