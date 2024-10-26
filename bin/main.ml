open Import

let all : _ Cmdliner.Cmd.t list =
  let terms =
    Runtest.commands
    @ [ Installed_libraries.command
      ; External_lib_deps.command
      ; Build_cmd.build
      ; Fmt.command
      ; Clean.command
      ; Install_uninstall.install
      ; Install_uninstall.uninstall
      ; Exec.command
      ; Subst.command
      ; Print_rules.command
      ; Utop.command
      ; Promotion.promote
      ; command_alias Printenv.command Printenv.term "printenv"
      ; Help.command
      ; Format_dune_file.command
      ; Upgrade.command
      ; Cache.command
      ; Top.command
      ; Ocaml_merlin.command
      ; Shutdown.command
      ; Diagnostics.command
      ; Monitor.command
      ]
  in
  let groups =
    [ Ocaml_cmd.group
    ; Coq.group
    ; Describe.group
    ; Describe.Show.group
    ; Rpc.group
    ; Internal.group
    ; Init.group
    ; Promotion.group
    ; Pkg.group
    ; Pkg.Alias.group
    ; Tools.group
    ]
  in
  terms @ groups
;;

(* Short reminders for the most used and useful commands *)
let common_commands_synopsis =
  Common.command_synopsis
    [ "build [--watch]"
    ; "runtest [--watch]"
    ; "exec NAME"
    ; "utop [DIR]"
    ; "install"
    ; "init project NAME [PATH] [--libs=l1,l2 --ppx=p1,p2 --inline-tests]"
    ]
;;

let info =
  let doc = "composable build system for OCaml" in
  Cmd.info
    "dune"
    ~doc
    ~envs:Common.envs
    ~version:
      (match Build_info.V1.version () with
       | None -> "n/a"
       | Some v -> Build_info.V1.Version.to_string v)
    ~man:
      [ `Blocks common_commands_synopsis
      ; `S "DESCRIPTION"
      ; `P
          {|Dune is a build system designed for OCaml projects only. It
              focuses on providing the user with a consistent experience and takes
              care of most of the low-level details of OCaml compilation. All you
              have to do is provide a description of your project and Dune will
              do the rest.
            |}
      ; `P
          {|The scheme it implements is inspired from the one used inside Jane
              Street and adapted to the open source world. It has matured over a
              long time and is used daily by hundreds of developers, which means
              that it is highly tested and productive.
            |}
      ; `Blocks Common.help_secs
      ; Common.examples
          [ "Initialise a new project named `foo'", "dune init project foo"
          ; "Build all targets in the current source tree", "dune build"
          ; "Run the executable named `bar'", "dune exec bar"
          ; "Run all tests in the current source tree", "dune runtest"
          ; "Install all components defined in the project", "dune install"
          ; "Remove all build artefacts", "dune clean"
          ]
      ]
;;

let cmd = Cmd.group info all

let exit_and_flush code =
  Console.finish ();
  exit (Exit_code.code code)
;;

let () =
  Dune_rules.Colors.setup_err_formatter_colors ();
  try
    match Cmd.eval_value cmd ~catch:false with
    | Ok _ -> exit_and_flush Success
    | Error _ -> exit_and_flush Error
  with
  | Scheduler.Run.Shutdown.E Requested -> exit_and_flush Success
  | Scheduler.Run.Shutdown.E (Signal _) -> exit_and_flush Signal
  | exn ->
    let exn = Exn_with_backtrace.capture exn in
    Dune_util.Report_error.report exn;
    exit_and_flush Error
;;
