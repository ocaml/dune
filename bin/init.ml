open Import
open Dune_init

(** {1 Helper functions} *)

(** {2 Cmdliner Argument Converters} *)

let atom_parser s =
  match Dune_lang.Atom.parse s with
  | Some s -> Ok s
  | None -> Error (`Msg "expected a valid dune atom")
;;

let atom_printer ppf a = Format.pp_print_string ppf (Dune_lang.Atom.to_string a)

let component_name_parser s =
  (* TODO refactor to use Lib_name.Local.conv *)
  let err_msg () =
    User_error.make
      [ Pp.textf "invalid component name `%s'" s; Lib_name.Local.valid_format_doc ]
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
;;

let project_name_parser s =
  (* TODO refactor Dune_project_name to be Stringlike *)
  match Dune_project_name.named Loc.none s with
  | v -> Ok v
  | exception User_error.E _ ->
    User_error.make
      [ Pp.textf "invalid project name `%s'" s
      ; Pp.text
          "Project names must start with a letter and be composed only of letters, \
           numbers, '-' or '_'"
      ]
    |> User_message.to_string
    |> fun m -> Error (`Msg m)
;;

let project_name_printer ppf p =
  Format.pp_print_string ppf (Dune_project_name.to_string_hum p)
;;

let atom_conv = Arg.conv (atom_parser, atom_printer)
let component_name_conv = Arg.conv (component_name_parser, atom_printer)
let project_name_conv = Arg.conv (project_name_parser, project_name_printer)

(** {2 Status reporting} *)

let print_completion kind name =
  let open Pp.O in
  Console.print_user_message
    (User_message.make
       [ Pp.tag User_message.Style.Ok (Pp.verbatim "Success")
         ++ Pp.textf ": initialized %s component named " kind
         ++ Pp.tag User_message.Style.Kwd (Pp.verbatim (Dune_lang.Atom.to_string name))
       ])
;;

(** {1 CLI} *)

let path =
  let docv = "PATH" in
  Arg.(value & pos 1 (some string) None & info [] ~docv)
;;

let context_cwd : Init_context.t Term.t =
  let+ builder = Common.Builder.term
  and+ path = path in
  let builder = Common.Builder.set_default_root_is_cwd builder true in
  let common, config = Common.init builder in
  let project_defaults = config.project_defaults in
  Scheduler.go_with_rpc_server ~common ~config (fun () ->
    Memo.run (Init_context.make path project_defaults))
;;

module Public_name = struct
  type t =
    | Use_name
    | Public_name of Public_name.t

  let public_name_to_string = function
    | Use_name -> "<default>"
    | Public_name p -> Public_name.to_string p
  ;;

  let public_name default_name = function
    | None -> None
    | Some Use_name -> Some (Public_name.of_name_exn default_name)
    | Some (Public_name n) -> Some n
  ;;

  let conv =
    let parser s =
      if String.is_empty s
      then Ok Use_name
      else (
        match Public_name.of_string_user_error (Loc.none, s) with
        | Ok n -> Ok (Public_name n)
        | Error e -> Error (`Msg (User_message.to_string e)))
    in
    let printer ppf public_name =
      Format.pp_print_string ppf (public_name_to_string public_name)
    in
    Arg.conv (parser, printer)
  ;;
end

let libraries =
  let docv = "LIBRARIES" in
  let doc = "A comma separated list of libraries on which the component depends" in
  Arg.(value & opt (list atom_conv) [] & info [ "libs" ] ~docv ~doc)
;;

let pps =
  let docv = "PREPROCESSORS" in
  let doc = "A comma separated list of ppx preprocessors used by the component" in
  Arg.(value & opt (list atom_conv) [] & info [ "ppx" ] ~docv ~doc)
;;

let public : Public_name.t option Term.t =
  let docv = "PUBLIC_NAME" in
  let doc =
    "If called with an argument, make the component public under the given PUBLIC_NAME. \
     If supplied without an argument, use NAME."
  in
  Arg.(
    value
    & opt ~vopt:(Some Public_name.Use_name) (some Public_name.conv) None
    & info [ "public" ] ~docv ~doc)
;;

let common : Component.Options.Common.t Term.t =
  let+ name =
    let docv = "NAME" in
    Arg.(required & pos 0 (some component_name_conv) None & info [] ~docv)
  and+ public = public
  and+ libraries = libraries
  and+ pps = pps in
  let public = Public_name.public_name name public in
  { Component.Options.Common.name; public; libraries; pps }
;;

let project_common : Component.Options.Common.t Term.t =
  let+ project_name =
    let docv = "NAME" in
    Arg.(required & pos 0 (some project_name_conv) None & info [] ~docv)
  and+ libraries = libraries
  and+ pps = pps in
  let public = Dune_project_name.to_string_hum project_name in
  let name =
    String.map
      ~f:(function
        | '-' -> '_'
        | c -> c)
      public
    |> Dune_lang.Atom.of_string
  in
  let public =
    Some (Dune_lang.Atom.of_string public |> Dune_init.Public_name.of_name_exn)
  in
  { Component.Options.Common.name; public; libraries; pps }
;;

let inline_tests : bool Term.t =
  let docv = "USE_INLINE_TESTS" in
  let doc =
    "Whether to use inline tests. Only applicable for $(b,library) and $(b,project) \
     components."
  in
  Arg.(value & flag & info [ "inline-tests" ] ~docv ~doc)
;;

let opt_default ~default term = Term.(const (Option.value ~default) $ term)

let executable =
  let doc = "A binary executable." in
  let man = [] in
  let kind = "executable" in
  Cmd.v (Cmd.info kind ~doc ~man)
  @@ let+ context = context_cwd
     and+ common = common in
     Component.init (Executable { context; common; options = () });
     print_completion kind common.name
;;

let library =
  let doc = "An OCaml library." in
  let man = [] in
  let kind = "library" in
  Cmd.v (Cmd.info kind ~doc ~man)
  @@ let+ context = context_cwd
     and+ common = common
     and+ inline_tests = inline_tests in
     Component.init (Library { context; common; options = { inline_tests } });
     print_completion kind common.name
;;

let test =
  let doc =
    "A test harness. (For inline tests, use the $(b,--inline-tests) flag along with the \
     other component kinds.)"
  in
  let man = [] in
  let kind = "test" in
  Cmd.v (Cmd.info kind ~doc ~man)
  @@ let+ context = context_cwd
     and+ common = common in
     Component.init (Test { context; common; options = () });
     print_completion kind common.name
;;

let project =
  let module Builder = Common.Builder in
  let doc =
    "A project is a predefined composition of components arranged in a standard \
     directory structure. The kind of project initialized is determined by the value of \
     the $(b,--kind) flag and defaults to an executable project, composed of a library, \
     an executable, and a test component."
  in
  let man = [] in
  Cmd.v (Cmd.info "project" ~doc ~man)
  @@ let+ common_builder = Builder.term
     and+ path = path
     and+ common = project_common
     and+ inline_tests = inline_tests
     and+ template =
       let docv = "PROJECT_KIND" in
       let doc =
         "The kind of project to initialize. Valid options are $(b,e[xecutable]) or \
          $(b,l[ibrary]). Defaults to $(b,executable). Only applicable for $(b,project) \
          components."
       in
       opt_default
         ~default:Component.Options.Project.Template.Exec
         Arg.(
           value
           & opt (some (enum Component.Options.Project.Template.commands)) None
           & info [ "kind" ] ~docv ~doc)
     and+ pkg =
       let docv = "PACKAGE_MANAGER" in
       let doc =
         "Which package manager to use. Valid options are $(b,o[pam]) or $(b,e[sy]). \
          Defaults to $(b,opam). Only applicable for $(b,project) components."
       in
       opt_default
         ~default:Component.Options.Project.Pkg.Opam
         Arg.(
           value
           & opt (some (enum Component.Options.Project.Pkg.commands)) None
           & info [ "pkg" ] ~docv ~doc)
     in
     let name =
       match common.public with
       | None -> Dune_lang.Atom.to_string common.name
       | Some public -> Dune_init.Public_name.to_string public
     in
     let context =
       let init_context = Init_context.make path in
       let root =
         match path with
         (* If a path is given, we use that for the root during project
            initialization, creating the path to it if needed. *)
         | Some path -> path
         (* Otherwise we will use the project's given name, and create a
            directory accordingly. *)
         | None -> name
       in
       let builder = Builder.set_root common_builder root in
       let (_ : Fpath.mkdir_p_result) = Fpath.mkdir_p root in
       let common, config = Common.init builder in
       let project_defaults = config.project_defaults in
       Scheduler.go_with_rpc_server ~common ~config (fun () ->
         Memo.run @@ init_context project_defaults)
     in
     Component.init
       (Project { context; common; options = { template; inline_tests; pkg } });
     print_completion "project" (Dune_lang.Atom.of_string name)
;;

let group =
  let doc = "Command group for initializing Dune components." in
  let synopsis =
    Common.command_synopsis
      [ "init project NAME [PATH] [OPTION]... "
      ; "init executable NAME [PATH] [OPTION]... "
      ; "init library NAME [PATH] [OPTION]... "
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
        {|If the optional $(b,PATH) is provided, it must be a path to a directory, and
        the component will be created there. Otherwise, it is created in a child of the
        current working directory, called $(b, NAME). To initialize a component in the
        current working directory, use `.` as the $(b,PATH).|}
    ; `P
        {|Any prefix of a $(b,COMMAND)'s name can be supplied in place of
        full name (as illustrated in the synopsis).|}
    ; `P
        {|For more details, see https://dune.readthedocs.io/en/stable/usage.html#initializing-components|}
    ; Common.examples
        [ ( {|Generate a project skeleton for an executable named `myproj' in a
            new directory named `myproj', depending on the bos library and
            using inline tests along with ppx_inline_test |}
          , {|dune init project myproj --libs bos --ppx ppx_inline_test --inline-tests|} )
        ; ( {|Configure an executable component named `myexe' in a dune file in the
            current directory|}
          , {|dune init executable myexe|} )
        ; ( {|Configure a library component named `mylib' in a dune file in the ./src
            directory depending on the core and cmdliner libraries, the ppx_let
            and ppx_inline_test preprocessors, and declared as using inline
            tests|}
          , {|dune init library mylib src --libs core,cmdliner --ppx ppx_let,ppx_inline_test --inline-tests|}
          )
        ; ( {|Configure a test component named `mytest' in a dune file in the
            ./test directory that depends on `mylib'|}
          , {|dune init test mytest test --libs mylib|} )
        ]
    ]
  in
  Cmd.group (Cmd.info "init" ~doc ~man) [ executable; project; library; test ]
;;
