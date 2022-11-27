open Stdune
open Import
open Dune_init

(** {1 Helper functions} *)

(** {2 Cmdliner Argument Converters} *)

let atom_parser s =
  match Dune_lang.Atom.parse s with
  | Some s -> Ok s
  | None -> Error (`Msg "expected a valid dune atom")

let atom_printer ppf a = Format.pp_print_string ppf (Dune_lang.Atom.to_string a)

let component_name_parser s =
  (* TODO refactor to use Lib_name.Local.conv *)
  let err_msg () =
    User_error.make
      [ Pp.textf "invalid component name `%s'" s
      ; Lib_name.Local.valid_format_doc
      ]
    |> User_message.to_string
    |> fun m -> `Msg m
  in
  let open Result.O in
  let* atom = atom_parser s in
  let* _ =
    match Lib_name.Local.of_string_opt s with
    | None -> Error (err_msg ())
    | Some s -> Ok s
  in
  Ok atom

let atom_conv = Arg.conv (atom_parser, atom_printer)

let component_name_conv = Arg.conv (component_name_parser, atom_printer)

let public_name_conv =
  let open Component.Options in
  let parser = function
    | "" -> Ok Use_name
    | s -> component_name_parser s |> Result.map ~f:(fun a -> Public_name a)
  in
  let printer ppf public_name =
    Format.pp_print_string ppf (public_name_to_string public_name)
  in
  Arg.conv (parser, printer)

(** {2 Status reporting} *)

let print_completion kind name =
  let open Pp.O in
  Console.print_user_message
    (User_message.make
       [ Pp.tag User_message.Style.Ok (Pp.verbatim "Success")
         ++ Pp.textf ": initialized %s component named " kind
         ++ Pp.tag User_message.Style.Kwd
              (Pp.verbatim (Dune_lang.Atom.to_string name))
       ])

(** {1 CLI} *)

let common : Component.Options.Common.t Term.t =
  let+ name =
    let docv = "NAME" in
    Arg.(required & pos 0 (some component_name_conv) None & info [] ~docv)
  and+ libraries =
    let docv = "LIBRARIES" in
    let doc =
      "A comma separated list of libraries on which the component depends"
    in
    Arg.(value & opt (list atom_conv) [] & info [ "libs" ] ~docv ~doc)
  and+ pps =
    let docv = "PREPROCESSORS" in
    let doc =
      "A comma separated list of ppx preprocessors used by the component"
    in
    Arg.(value & opt (list atom_conv) [] & info [ "ppx" ] ~docv ~doc)
  in
  { Component.Options.Common.name; libraries; pps }

let context : Init_context.t Term.t =
  let+ common_term = Common.term_with_default_root_is_cwd
  and+ path =
    let docv = "PATH" in
    Arg.(value & pos 1 (some string) None & info [] ~docv)
  in
  let config = Common.init common_term in
  Scheduler.go ~common:common_term ~config (fun () ->
      Memo.run (Init_context.make path))

let public : Component.Options.public_name option Term.t =
  let docv = "PUBLIC_NAME" in
  let doc =
    "If called with an argument, make the component public under the given \
     PUBLIC_NAME. If supplied without an argument, use NAME."
  in
  Arg.(
    value
    & opt ~vopt:(Some Component.Options.Use_name) (some public_name_conv) None
    & info [ "public" ] ~docv ~doc)

let inline_tests : bool Term.t =
  let docv = "USE_INLINE_TESTS" in
  let doc =
    "Whether to use inline tests. Only applicable for $(b,library) and \
     $(b,project) components."
  in
  Arg.(value & flag & info [ "inline-tests" ] ~docv ~doc)

let opt_default ~default term = Term.(const (Option.value ~default) $ term)

let executable =
  let doc = "A binary executable." in
  let man = [] in
  let kind = "executable" in
  Cmd.v (Cmd.info kind ~doc ~man)
  @@ let+ context = context
     and+ common = common
     and+ public = public in
     Component.init (Executable { context; common; options = { public } });
     print_completion kind common.name

let library =
  let doc = "An OCaml library." in
  let man = [] in
  let kind = "library" in
  Cmd.v (Cmd.info kind ~doc ~man)
  @@ let+ context = context
     and+ common = common
     and+ public = public
     and+ inline_tests = inline_tests in
     Component.init
       (Library { context; common; options = { public; inline_tests } });
     print_completion kind common.name

let test =
  let doc =
    "A test harness. (For inline tests, use the $(b,--inline-tests) flag along \
     with the other component kinds.)"
  in
  let man = [] in
  let kind = "test" in
  Cmd.v (Cmd.info kind ~doc ~man)
  @@ let+ context = context
     and+ common = common in
     Component.init (Test { context; common; options = () });
     print_completion kind common.name

let project =
  let open Component.Options in
  let doc =
    "A project is a predefined composition of components arranged in a \
     standard directory structure. The kind of project initialized is \
     determined by the value of the $(b,--kind) flag and defaults to an \
     executable project, composed of a library, an executable, and a test \
     component."
  in
  let man = [] in
  Cmd.v (Cmd.info "project" ~doc ~man)
  @@ let+ context = context
     and+ common = common
     and+ inline_tests = inline_tests
     and+ template =
       let docv = "PROJECT_KIND" in
       let doc =
         "The kind of project to initialize. Valid options are \
          $(b,e[xecutable]) or $(b,l[ibrary]). Defaults to $(b,executable). \
          Only applicable for $(b,project) components."
       in
       opt_default ~default:Project.Template.Exec
         Arg.(
           value
           & opt (some (enum Project.Template.commands)) None
           & info [ "kind" ] ~docv ~doc)
     and+ pkg =
       let docv = "PACKAGE_MANAGER" in
       let doc =
         "Which package manager to use. Valid options are $(b,o[pam]) or \
          $(b,e[sy]). Defaults to $(b,opam). Only applicable for $(b,project) \
          components."
       in
       opt_default ~default:Project.Pkg.Opam
         Arg.(
           value
           & opt (some (enum Project.Pkg.commands)) None
           & info [ "pkg" ] ~docv ~doc)
     in
     Component.init
       (Project { context; common; options = { template; inline_tests; pkg } });
     print_completion "project" common.name

let group =
  let doc = "Command group for initializing dune components" in
  let synopsis =
    Common.command_synopsis
      [ "init proj NAME [PATH] [OPTION]... "
      ; "init exec NAME [PATH] [OPTION]... "
      ; "init lib NAME [PATH] [OPTION]... "
      ; "init test NAME [PATH] [OPTION]... "
      ]
  in
  let man =
    [ `Blocks synopsis
    ; `S "DESCRIPTION"
    ; `P
        {|$(b,dune init COMPONENT NAME [PATH] [OPTION]...) initializes a new dune
        configuration for a component of the kind specified by the subcommand
        $(b,COMPONENT), named $(b,NAME), with fields determined by the supplied
        $(b,OPTION)s.|}
    ; `P
        {|Run a subcommand with $(b, --help) for for details on it's supported arguments|}
    ; `P
        {|If the optional $(b,PATH) is provided, the component will be created
        there. Otherwise, it is created in the current working directory.|}
    ; `P
        {|Any prefix of a $(b,COMMAND)'s name can be supplied in place of
        full name (as illustrated in the synopsis).|}
    ; `P
        {|For more details, see https://dune.readthedocs.io/en/stable/usage.html#initializing-components|}
    ; Common.examples
        [ ( {|Generate a project skeleton for an executable named `myproj' in a
            new directory named `myproj', depending on the bos library and
            using inline tests along with ppx_inline_test |}
          , {|dune init proj myproj --libs bos --ppx ppx_inline_test --inline-tests|}
          )
        ; ( {|Configure an executable component named `myexe' in a dune file in the
            current directory|}
          , {|dune init exe myexe|} )
        ; ( {|Configure a library component named `mylib' in a dune file in the ./src
            directory depending on the core and cmdliner libraries, the ppx_let
            and ppx_inline_test preprocessors, and declared as using inline
            tests|}
          , {|dune init lib mylib src --libs core,cmdliner --ppx ppx_let,ppx_inline_test --inline-tests|}
          )
        ; ( {|Configure a test component named `mytest' in a dune file in the
            ./test directory that depends on `mylib'|}
          , {|dune init test mytest test --libs mylib|} )
        ]
    ]
  in
  Cmd.group (Cmd.info "init" ~doc ~man) [ executable; project; library; test ]
