open Jbuilder
open Import
open Jbuilder_cmdliner.Cmdliner

(* Things in src/ don't depend on cmdliner to speed up the bootstrap, so we set this
   reference here *)
let () = suggest_function := Jbuilder_cmdliner.Cmdliner_suggest.value

let (>>=) = Future.(>>=)

type common =
  { concurrency    : int
  ; debug_rules    : bool
  ; debug_actions  : bool
  ; debug_dep_path : bool
  ; debug_findlib  : bool
  ; dev_mode       : bool
  ; workspace_file : string option
  ; root           : string
  ; target_prefix  : string
  ; only_packages  : String_set.t option
  }

let prefix_target common s = common.target_prefix ^ s

let set_common c =
  Clflags.concurrency := c.concurrency;
  Clflags.debug_rules := c.debug_rules;
  Clflags.debug_actions := c.debug_actions;
  Clflags.debug_dep_path := c.debug_dep_path;
  Clflags.debug_findlib := c.debug_findlib;
  Clflags.dev_mode := c.dev_mode;
  Printf.eprintf "Workspace root: %s\n" c.root;
  if c.root <> Filename.current_dir_name then
    Sys.chdir c.root

module Main = struct
  include Jbuilder.Main

  let setup ~log ?filter_out_optional_stanzas_with_missing_deps common =
    setup
      ~log
      ?workspace_file:common.workspace_file
      ?only_packages:common.only_packages
      ?filter_out_optional_stanzas_with_missing_deps ()
end

let do_build (setup : Main.setup) targets =
  Build_system.do_build_exn setup.build_system targets

type ('a, 'b) walk_result =
  | Cont of 'a
  | Stop of 'b

let rec walk_parents dir ~init ~f =
  match f init dir with
  | Stop x -> Stop x
  | Cont x ->
    let parent = Filename.dirname dir in
    if parent = dir then
      Cont x
    else
      walk_parents parent ~init:x ~f

let find_root () =
  let cwd = Sys.getcwd () in
  let rec loop counter ~candidates ~to_cwd dir =
    let files = Sys.readdir dir |> Array.to_list |> String_set.of_list in
    if String_set.mem "jbuild-workspace" files then
      cont counter ~candidates:((0, dir, to_cwd) :: candidates) dir ~to_cwd
    else if String_set.exists files ~f:(fun fn ->
        String.is_prefix fn ~prefix:"jbuild-workspace") then
      cont counter ~candidates:((1, dir, to_cwd) :: candidates) dir ~to_cwd
    else
      cont counter ~candidates dir ~to_cwd
  and cont counter ~candidates ~to_cwd dir =
    if counter > String.length cwd then
      candidates
    else
      let parent = Filename.dirname dir in
      if parent = dir then
        candidates
      else
        let base = Filename.basename dir in
        loop (counter + 1) parent ~candidates ~to_cwd:(base :: to_cwd)
  in
  match loop 0 ~candidates:[] ~to_cwd:[] cwd with
  | [] -> (cwd, [])
  | l ->
    let lowest_priority =
      List.fold_left l ~init:max_int ~f:(fun acc (prio, _, _) ->
        min acc prio)
    in
    match List.find l ~f:(fun (prio, _, _) -> prio = lowest_priority) with
    | None -> assert false
    | Some (_, dir, to_cwd) -> (dir, to_cwd)

let copts_sect = "COMMON OPTIONS"
let help_secs =
  [ `S copts_sect
  ; `P "These options are common to all commands."
  ; `S "MORE HELP"
  ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
  ; `S "BUGS"
  ; `P "Check bug reports at https://github.com/janestreet/jbuilder/issues"
  ]

let common =
  let make
        concurrency
        only_packages
        debug_rules
        debug_actions
        debug_dep_path
        debug_findlib
        dev_mode
        workspace_file
        root
    =
    let root, to_cwd =
      match root with
      | Some dn -> (dn, [])
      | None -> find_root ()
    in
    { concurrency
    ; debug_rules
    ; debug_actions
    ; debug_dep_path
    ; debug_findlib
    ; dev_mode
    ; workspace_file
    ; root
    ; target_prefix = String.concat ~sep:"" (List.map to_cwd ~f:(sprintf "%s/"))
    ; only_packages =
        Option.map only_packages
          ~f:(fun s -> String_set.of_list (String.split s ~on:','))
    }
  in
  let docs = copts_sect in
  let concurrency =
    Arg.(value
         & opt int !Clflags.concurrency
         & info ["j"] ~docs ~docv:"JOBS"
             ~doc:{|Run no more than $(i,JOBS) commands simultaneously.|}
        )
  in
  let only_packages =
    Arg.(value
         & opt (some string) None
         & info ["only-packages"] ~docs ~docv:"PACKAGES"
             ~doc:{|Ignore stanzas referring to a package that is not in $(b,PACKAGES).
                    $(b,PACKAGES) is a coma-separated list of package name. You need to
                    use this option in your $(i,<package>.opam) file if your project
                    contains several packages.|}
        )
  in
  let drules =
    Arg.(value
         & flag
         & info ["debug-rules"] ~docs
             ~doc:"Print all internal rules."
        )
  in
  let dactions =
    Arg.(value
         & flag
         & info ["debug-actions"] ~docs
             ~doc:"Print out internal actions."
        )
  in
  let ddep_path =
    Arg.(value
         & flag
         & info ["debug-dependency-path"] ~docs
             ~doc:{|In case of error, print the dependency path from
                    the targets on the command line to the rule that failed.
                  |})
  in
  let dfindlib =
    Arg.(value
         & flag
         & info ["debug-findlib"] ~docs
             ~doc:{|Debug the findlib sub-system.|})
  in
  let dev =
    Arg.(value
         & flag
         & info ["dev"] ~docs
             ~doc:{|Use stricter compilation flags by default.|})
  in
  let workspace_file =
    Arg.(value
         & opt (some file) None
         & info ["workspace"] ~docs ~docv:"FILE"
             ~doc:"Use this specific workspace file instead of looking it up.")
  in
  let root =
    Arg.(value
         & opt (some dir) None
         & info ["root"] ~docs ~docv:"DIR"
             ~doc:{|Use this directory as workspace root instead of guessing it.
                   Note that this option doesn't change the interpretation of
                   targets given on the command line. It is only intended
                    for scripts.|})
  in
  Term.(const make
        $ concurrency
        $ only_packages
        $ drules
        $ dactions
        $ ddep_path
        $ dfindlib
        $ dev
        $ workspace_file
        $ root
       )

let installed_libraries =
  let doc = "Print out libraries installed on the system." in
  let go common =
    set_common common;
    Future.Scheduler.go ~log:(Log.create ())
      (Context.default () >>= fun ctx ->
       let findlib = ctx.findlib in
       let pkgs = Findlib.all_packages findlib in
       let max_len = List.longest_map pkgs ~f:(fun p -> p.name) in
       List.iter pkgs ~f:(fun pkg ->
         let ver =
           match pkg.Findlib.version with
           | "" -> "n/a"
           | v  -> v
         in
         Printf.printf "%-*s (version: %s)\n" max_len pkg.name ver);
       Future.return ())
  in
  ( Term.(const go
          $ common)
  , Term.info "installed-libraries" ~doc
  )

let resolve_package_install setup pkg =
  match Main.package_install_file setup pkg with
  | Ok path -> path
  | Error () ->
    die "Unknown package %s!%s" pkg (hint pkg (String_map.keys setup.packages))

type target =
  | File  of Path.t
  | Alias of Path.t * Alias.t

let resolve_targets common (setup : Main.setup) user_targets =
  match user_targets with
  | [] -> []
  | _ ->
    let targets =
      List.concat_map user_targets ~f:(fun s ->
        if String.is_prefix s ~prefix:"@" then
          let s = String.sub s ~pos:1 ~len:(String.length s - 1) in
          let path = Path.relative Path.root (prefix_target common s) in
          if Path.is_root path then
            die "@ on the command line must be followed by a valid alias name"
          else
            let dir = Path.parent path in
            let name = Path.basename path in
            [Alias (path, Alias.make ~dir name)]
        else
          let path = Path.relative Path.root (prefix_target common s) in
          let can't_build path =
            die "Don't know how to build %s" (Path.to_string path)
          in
          if not (Path.is_local path) then
            [File path]
          else if Path.is_in_build_dir path then begin
            if Build_system.is_target setup.build_system path then
              [File path]
            else
              can't_build path
          end else
            match
              let l =
                List.filter_map setup.contexts ~f:(fun ctx ->
                    let path = Path.append ctx.Context.build_dir path in
                    if Build_system.is_target setup.build_system path then
                      Some (File path)
                    else
                      None)
              in
              if Build_system.is_target setup.build_system path ||
                 Path.exists path then
                File path :: l
              else
                l
            with
            | [] -> can't_build path
            | l  -> l
        )
    in
    Printf.printf "Actual targets:\n";
    List.iter targets ~f:(function
      | File path ->
        Printf.printf "- %s\n" (Path.to_string path)
      | Alias (path, _) ->
        Printf.printf "- alias %s\n" (Path.to_string path));
    flush stdout;
    List.map targets ~f:(function
      | File path -> path
      | Alias (_, alias) -> Alias.file alias)

let build_targets =
  let doc = "Build the given targets." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Targets starting with a $(b,@) are interpreted as aliases.|}
    ; `Blocks help_secs
    ]
  in
  let name_ = Arg.info [] ~docv:"TARGET" in
  let go common targets =
    set_common common;
    let log = Log.create () in
    Future.Scheduler.go ~log
      (Main.setup ~log common >>= fun setup ->
       let targets = resolve_targets common setup targets in
       do_build setup targets) in
  ( Term.(const go
          $ common
          $ Arg.(non_empty & pos_all string [] name_))
  , Term.info "build" ~doc ~man)

let runtest =
  let doc = "Run tests." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|This is a short-hand for calling:|}
    ; `Pre {|  jbuilder build @runtest|}
    ; `Blocks help_secs
    ]
  in
  let name_ = Arg.info [] ~docv:"DIR" in
  let go common dirs =
    set_common common;
    let log = Log.create () in
    Future.Scheduler.go ~log
      (Main.setup ~log common >>= fun setup ->
       let targets =
         List.map dirs ~f:(fun dir ->
           let dir = Path.(relative root) (prefix_target common dir) in
           Alias.file (Alias.runtest ~dir))
       in
       do_build setup targets) in
  ( Term.(const go
          $ common
          $ Arg.(value & pos_all string ["."] name_))
  , Term.info "runtest" ~doc ~man)

let format_external_libs libs =
  String_map.bindings libs
  |> List.map ~f:(fun (name, kind) ->
    match (kind : Build.lib_dep_kind) with
    | Optional -> sprintf "- %s (optional)" name
    | Required -> sprintf "- %s" name)
  |> String.concat ~sep:"\n"

let external_lib_deps =
  let doc = "Print out external libraries needed to build the given targets." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Print out the external libraries needed to build the given targets.|}
    ; `P {|The output of $(b,jbuild external-lib-deps @install) should be included
           in what is written in your $(i,<package>.opam) file.|}
    ; `Blocks help_secs
    ]
  in
  let go common only_missing targets =
    set_common common;
    let log = Log.create () in
    Future.Scheduler.go ~log
      (Main.setup ~log common ~filter_out_optional_stanzas_with_missing_deps:false
       >>= fun setup ->
       let targets = resolve_targets common setup targets in
       let failure =
         String_map.fold ~init:false
           (Build_system.all_lib_deps_by_context setup.build_system targets)
           ~f:(fun ~key:context_name ~data:lib_deps acc ->
             let internals =
               Jbuild_types.Stanza.lib_names
                 (match String_map.find context_name setup.Main.stanzas with
                  | None -> assert false
                  | Some x -> x)
             in
             let externals =
               String_map.filter lib_deps ~f:(fun name _ ->
                 not (String_set.mem name internals))
             in
             if only_missing then begin
               let context =
                 match List.find setup.contexts ~f:(fun c -> c.name = context_name) with
                 | None -> assert false
                 | Some c -> c
               in
               let missing =
                 String_map.filter externals ~f:(fun name _ ->
                   not (Findlib.available context.findlib name ~required_by:[]))
               in
               if String_map.is_empty missing then
                 acc
               else begin
                 Format.eprintf
                   "@{<error>Error@}: The following required libraries are missing \
                    in the %s context:\n\
                    %s@."
                   context_name
                   (format_external_libs missing);
                 true
               end
             end else begin
               Printf.printf
                 "These are the external library dependencies in the %s context:\n\
                  %s\n%!"
                 context_name
                 (format_external_libs externals);
               acc
             end)
       in
       if failure then die "";
       Future.return ())
  in
  ( Term.(const go
          $ common
          $ Arg.(value
                 & flag
                 & info ["missing"]
                     ~doc:{|Only print out missing dependencies|})
          $ Arg.(non_empty
                 & pos_all string []
                 & Arg.info [] ~docv:"TARGET"))
  , Term.info "external-lib-deps" ~doc ~man)

let opam_installer () =
  match Bin.which "opam-installer" with
  | None ->
    die "\
Sorry, you need the opam-installer tool to be able to install or
uninstall packages.

I couldn't find the opam-installer binary :-("
  | Some fn -> fn

let get_prefix context ~from_command_line =
  match from_command_line with
  | Some p -> Future.return (Path.of_string p)
  | None -> Context.install_prefix context

let install_uninstall ~what =
  let doc =
    sprintf "%s packages using opam-installer." (String.capitalize_ascii what)
  in
  let name_ = Arg.info [] ~docv:"PACKAGE" in
  let go common prefix pkgs =
    set_common common;
    let opam_installer = opam_installer () in
    let log = Log.create () in
    Future.Scheduler.go ~log
      (Main.setup ~log common >>= fun setup ->
       let pkgs =
         match pkgs with
         | [] -> String_map.keys setup.packages
         | l  -> l
       in
       let install_files, missing_install_files =
         List.concat_map pkgs ~f:(fun pkg ->
           let fn = resolve_package_install setup pkg in
           List.map setup.contexts ~f:(fun ctx ->
             let fn = Path.append ctx.Context.build_dir fn in
             if Path.exists fn then
               Inl (ctx, fn)
             else
               Inr fn))
         |> List.partition_map ~f:(fun x -> x)
       in
       if missing_install_files <> [] then begin
         die "The following <package>.install are missing:\n\
              %s\n\
              You need to run: jbuilder build @install"
           (String.concat ~sep:"\n"
              (List.map missing_install_files
                 ~f:(fun p -> sprintf "- %s" (Path.to_string p))))
       end;
       (match setup.contexts, prefix with
        | _ :: _ :: _, Some _ ->
          die "Cannot specify --prefix when installing into multiple contexts!"
        | _ -> ());
       let module CMap = Map.Make(Context) in
       let install_files_by_context = CMap.of_alist_multi install_files |> CMap.bindings in
       Future.all_unit
         (List.map install_files_by_context ~f:(fun (context, install_files) ->
            get_prefix context ~from_command_line:prefix >>= fun prefix ->
            Future.all_unit
              (List.map install_files ~f:(fun path ->
                   Future.run Strict (Path.to_string opam_installer)
                     [ sprintf "-%c" what.[0]
                     ; "--prefix"
                     ; Path.to_string prefix
                     ; Path.to_string path
                     ])))))
  in
  ( Term.(const go
          $ common
          $ Arg.(value & opt (some dir) None & info ["prefix"])
          $ Arg.(value & pos_all string [] name_))
  , Term.info what ~doc ~man:help_secs)

let install   = install_uninstall ~what:"install"
let uninstall = install_uninstall ~what:"uninstall"

let exec =
  let doc =
    "Execute a command in a similar environment as if installation was performed."
  in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|$(b,jbuilder exec -- COMMAND) should behave in the same way as if you do:|}
    ; `Pre "  \\$ jbuilder install\n\
           \  \\$ COMMAND"
    ; `P {|In particular if you run $(b,jbuilder exec ocaml), you will have access
           to the libraries defined in the workspace using your usual directives
           ($(b,#require) for instance)|}
    ; `Blocks help_secs
    ]
  in
  let go common context prog args =
    set_common common;
    let log = Log.create () in
    Future.Scheduler.go ~log
      (Main.setup ~log common >>= fun setup ->
       let context =
         match List.find setup.contexts ~f:(fun c -> c.name = context) with
         | Some ctx -> ctx
         | None ->
           Format.eprintf "@{<Error>Error@}: Context %S not found!@." context;
           die ""
       in
       match Context.which context prog with
       | None ->
         Format.eprintf "@{<Error>Error@}: Program %S not found!@." prog;
         die ""
       | Some real_prog ->
         let real_prog = Path.to_string real_prog     in
         let env       = Context.env_for_exec context in
         if Sys.win32 then
           Future.run ~env Strict real_prog (prog :: args)
         else
           Unix.execve real_prog (Array.of_list (prog :: args)) env
      )
  in
  ( Term.(const go
          $ common
          $ Arg.(value
                 & opt string "default"
                 & info ["context"] ~docv:"CONTEXT"
                     ~doc:{|Run the command in this build context.|}
                )
          $ Arg.(required
                 & pos 0 (some string) None (Arg.info [] ~docv:"PROG"))
          $ Arg.(value
                 & pos_right 0 string [] (Arg.info [] ~docv:"ARGS"))
         )
  , Term.info "exec" ~doc ~man)


let all =
  [ installed_libraries
  ; external_lib_deps
  ; build_targets
  ; runtest
  ; install
  ; uninstall
  ; exec
  ]

let default =
  let doc = "composable build system for OCaml" in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ common))
  , Term.info "jbuilder" ~doc
      ~man:
        [ `S "DESCRIPTION"
        ; `P {|Jbuilder is a build system designed for OCaml projects only. It
               focuses on providing the user with a consistent experience and takes
               care of most of the low-level details of OCaml compilation. All you
               have to do is provide a description of your project and Jbuilder will
               do the rest.
             |}
        ; `P {|The scheme it implements is inspired from the one used inside Jane
               Street and adapted to the open source world. It has matured over a
               long time and is used daily by hundred of developpers, which means
               that it is highly tested and productive.
             |}
        ; `Blocks help_secs
        ]
  )

let () =
  Ansi_color.setup_err_formatter_colors ();
  try
    match Term.eval_choice default all ~catch:false with
    | `Error _ -> exit 1
    | _ -> exit 0
  with exn ->
    Format.eprintf "%a@?" (Main.report_error ?map_fname:None) exn;
    exit 1
