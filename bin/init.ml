open Stdune
open Import
open Dune.Dune_init

(* TODO(shonfeder): Remove when nested subcommands are available *)
let validate_component_options kind unsupported_options =
  let report_invalid_option = function
    | _, false -> () (* The option wasn't supplied *)
    | option_name, true ->
      User_error.raise
        [ Pp.textf "The %s component does not support the %s option"
            (Kind.to_string kind) option_name
        ]
  in
  List.iter ~f:report_invalid_option unsupported_options

let doc = "Initialize dune components"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune init {library,executable,test,project} NAME [PATH]) initialize
         a new dune component of the specified kind, named $(b,NAME), with
         fields determined by the supplied options.|}
  ; `P
      {|Any prefix of the component kinds can be supplied, e.g., $(b,dune init
         proj myproject).|}
  ; `P
      {|If the optional $(b,PATH) is provided, the component will be created
         there. Otherwise, it is created in the current working directory.|}
  ; `P
      {|The command can be used to add stanzas to existing dune files as
         well as for creating new dune files and basic component templates.|}
  ; `S "EXAMPLES"
  ; `Pre
      {|
Define an executable component named 'myexe' in a dune file in the
current directory:

          dune init exe myexe

Define a library component named 'mylib' in a dune file in the ./src
directory depending on the core and cmdliner libraries, the ppx_let
and ppx_inline_test preprocessors, and declared as using inline tests:

          dune init lib mylib src --libs core,cmdliner --ppx ppx_let,ppx_inline_test --inline-tests

Define a library component named mytest in a dune file in the ./test
directory that depends on mylib:

        dune init test myexe test --libs mylib|}
  ]

let info = Term.info "init" ~doc ~man

let term =
  let+ common_term = Common.term
  and+ kind =
    (* TODO(shonfeder): Replace with nested subcommand once we have support for
       that *)
    Arg.(
      required
      & pos 0 (some (enum Kind.commands)) None
      & info [] ~docv:"INIT_KIND")
  and+ name = Arg.(required & pos 1 (some string) None & info [] ~docv:"NAME")
  and+ path = Arg.(value & pos 2 (some string) None & info [] ~docv:"PATH")
  and+ libraries =
    Arg.(
      value
      & opt (list string) []
      & info [ "libs" ] ~docv:"LIBRARIES"
          ~doc:"Libraries on which the component depends")
  and+ pps =
    Arg.(
      value
      & opt (list string) []
      & info [ "ppx" ] ~docv:"PREPROCESSORS"
          ~doc:"ppx preprocessors used by the component")
  and+ public =
    (* TODO(shonfeder): Move to subcommands {lib, exe} once implemented *)
    Arg.(
      value
      & opt ~vopt:(Some "") (some string) None
      & info [ "public" ] ~docv:"PUBLIC_NAME"
          ~doc:
            "If called with an argument, make the component public under the \
             given PUBLIC_NAME. If supplied without an argument, use NAME.")
  and+ inline_tests =
    (* TODO Move to subcommand lib once implemented *)
    Arg.(
      value & flag
      & info [ "inline-tests" ] ~docv:"USE_INLINE_TESTS"
          ~doc:
            "Whether to use inline tests. Only applicable for $(b,library) and \
             $(b,project) components.")
  and+ template =
    Arg.(
      value
      & opt (some (enum Component.Options.Project.Template.commands)) None
      & info [ "kind" ] ~docv:"PROJECT_KIND"
          ~doc:
            "The kind of project to initialize. Valid options are \
             $(b,e[xecutable]) or $(b,l[ibrary]). Defaults to $(b,executable). \
             Only applicable for $(b,project) components.")
  and+ pkg =
    Arg.(
      value
      & opt (some (enum Component.Options.Project.Pkg.commands)) None
      & info [ "pkg" ] ~docv:"PACKAGE_MANAGER"
          ~doc:
            "Which package manager to use. Valid options are $(b,o[pam]) or \
             $(b,e[sy]). Defaults to $(b,opam). Only applicable for \
             $(b,project) components.")
  in
  validate_component_name name;
  Common.set_common common_term ~targets:[];
  let open Component in
  let context = Init_context.make path in
  let common : Options.Common.t = { name; libraries; pps } in
  let given_public = Option.is_some public in
  let given_pkg = Option.is_some pkg in
  let given_template = Option.is_some template in
  let pkg = Option.value pkg ~default:Options.Project.Pkg.Opam in
  let template = Option.value template ~default:Options.Project.Template.Exec in
  (* for the [kind] of initialization *)
  let check_unsupported_options = validate_component_options kind in
  ( match kind with
  | Kind.Executable ->
    check_unsupported_options
      [ ("inline-tests", inline_tests)
      ; ("kind", given_template)
      ; ("pkg", given_pkg)
      ];
    init @@ Executable { context; common; options = { public } }
  | Kind.Library ->
    check_unsupported_options [ ("kind", given_template); ("pkg", given_pkg) ];
    init @@ Library { context; common; options = { public; inline_tests } }
  | Kind.Project ->
    check_unsupported_options [ ("public", given_public) ];
    init
    @@ Project { context; common; options = { inline_tests; pkg; template } }
  | Kind.Test ->
    check_unsupported_options
      [ ("public", given_public)
      ; ("inline-tests", inline_tests)
      ; ("kind", given_template)
      ; ("pkg", given_pkg)
      ];
    init @@ Test { context; common; options = () } );
  print_completion kind name

let command = (term, info)
