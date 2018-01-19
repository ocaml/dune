open Jbuilder
open Import
open Jbuilder_cmdliner.Cmdliner

(* Things in src/ don't depend on cmdliner to speed up the bootstrap, so we set this
   reference here *)
let () = suggest_function := Jbuilder_cmdliner.Cmdliner_suggest.value

let (>>=) = Future.(>>=)
let (>>|) = Future.(>>|)

type common =
  { concurrency      : int
  ; debug_dep_path   : bool
  ; debug_findlib    : bool
  ; debug_backtraces : bool
  ; dev_mode         : bool
  ; verbose          : bool
  ; workspace_file   : string option
  ; root             : string
  ; target_prefix    : string
  ; only_packages    : String_set.t option
  ; capture_outputs  : bool
  ; x                : string option
  ; diff_command     : string option
  ; auto_promote     : bool
  ; force            : bool
  ; (* Original arguments for the external-lib-deps hint *)
    orig_args        : string list
  }

let prefix_target common s = common.target_prefix ^ s

let set_common c ~targets =
  Clflags.concurrency := c.concurrency;
  Clflags.debug_dep_path := c.debug_dep_path;
  Clflags.debug_findlib := c.debug_findlib;
  Clflags.debug_backtraces := c.debug_backtraces;
  Clflags.dev_mode := c.dev_mode;
  Clflags.verbose := c.verbose;
  Clflags.capture_outputs := c.capture_outputs;
  if c.root <> Filename.current_dir_name then
    Sys.chdir c.root;
  Clflags.workspace_root := Sys.getcwd ();
  Clflags.diff_command := c.diff_command;
  Clflags.auto_promote := c.auto_promote;
  Clflags.force := c.force;
  Clflags.external_lib_deps_hint :=
    List.concat
      [ ["jbuilder"; "external-lib-deps"; "--missing"]
      ; c.orig_args
      ; targets
      ]

let restore_cwd_and_execve common prog argv env =
  let prog =
    if Filename.is_relative prog then
      Filename.concat common.root prog
    else
      prog
  in
  Sys.chdir initial_cwd;
  if Sys.win32 then
    let pid = Unix.create_process_env prog argv env
                Unix.stdin Unix.stdout Unix.stderr
    in
    match snd (Unix.waitpid [] pid) with
    | WEXITED   0 -> ()
    | WEXITED   n -> exit n
    | WSIGNALED _ -> exit 255
    | WSTOPPED  _ -> assert false
  else
    Unix.execve prog argv env

module Main = struct
  include Jbuilder.Main

  let setup ~log ?filter_out_optional_stanzas_with_missing_deps common =
    setup
      ~log
      ?workspace_file:common.workspace_file
      ?only_packages:common.only_packages
      ?filter_out_optional_stanzas_with_missing_deps
      ?x:common.x
      ()
end

type target =
  | File      of Path.t
  | Alias_rec of Path.t

let request_of_targets (setup : Main.setup) targets =
  let open Build.O in
  let contexts = List.map setup.contexts ~f:(fun c -> c.Context.name) in
  List.fold_left targets ~init:(Build.return ()) ~f:(fun acc target ->
    acc >>>
    match target with
    | File path -> Build.path path
    | Alias_rec path ->
      let dir = Path.parent path in
      let name = Path.basename path in
      let contexts, dir =
        match Path.extract_build_context dir with
        | None -> (contexts, dir)
        | Some ("install", _) ->
          die "Invalid alias: %s.\n\
               There are no aliases in _build/install."
            (Path.to_string_maybe_quoted path)
        | Some (ctx, dir) -> ([ctx], dir)
      in
      Build_system.Alias.dep_rec_multi_contexts ~dir ~name
        ~file_tree:setup.file_tree ~contexts)

let do_build (setup : Main.setup) targets =
  Build_system.do_build_exn setup.build_system
    ~request:(request_of_targets setup targets)

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
  let dump_opt name value =
    match value with
    | None -> []
    | Some s -> [name; s]
  in
  let make
        concurrency
        debug_dep_path
        debug_findlib
        debug_backtraces
        dev_mode
        verbose
        no_buffer
        workspace_file
        diff_command
        auto_promote
        force
        (root, only_packages, orig)
        x
    =
    let root, to_cwd =
      match root with
      | Some dn -> (dn, [])
      | None -> find_root ()
    in
    let orig_args =
      List.concat
        [ if dev_mode then ["--dev"] else []
        ; dump_opt "--workspace" workspace_file
        ; orig
        ]
    in
    { concurrency
    ; debug_dep_path
    ; debug_findlib
    ; debug_backtraces
    ; dev_mode
    ; verbose
    ; capture_outputs = not no_buffer
    ; workspace_file
    ; root
    ; orig_args
    ; target_prefix = String.concat ~sep:"" (List.map to_cwd ~f:(sprintf "%s/"))
    ; diff_command
    ; auto_promote
    ; force
    ; only_packages =
        Option.map only_packages
          ~f:(fun s -> String_set.of_list (String.split s ~on:','))
    ; x
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
                    $(b,PACKAGES) is a comma-separated list of package names.
                    Note that this has the same effect as deleting the relevant stanzas
                    from jbuild files. It is mostly meant for releases.
                    During development, it is likely that what you want instead is to
                    build a particular $(b,<package>.install) target.|}
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
  let dbacktraces =
    Arg.(value
         & flag
         & info ["debug-backtraces"] ~docs
             ~doc:{|Always print exception backtraces.|})
  in
  let dev =
    Arg.(value
         & flag
         & info ["dev"] ~docs
             ~doc:{|Use stricter compilation flags by default.|})
  in
  let verbose =
    Arg.(value
         & flag
         & info ["verbose"] ~docs
             ~doc:"Print detailed information about commands being run")
  in
  let no_buffer =
    Arg.(value
         & flag
         & info ["no-buffer"] ~docs ~docv:"DIR"
             ~doc:{|Do not buffer the output of commands executed by jbuilder.
                    By default jbuilder buffers the output of subcommands, in order
                    to prevent interleaving when multiple commands are executed
                    in parallel. However, this can be an issue when debugging
                    long running tests. With $(b,--no-buffer), commands have direct
                    access to the terminal. Note that as a result their output won't
                    be captured in the log file.

                    You should use this option in conjunction with $(b,-j 1),
                    to avoid interleaving. Additionally you should use
                    $(b,--verbose) as well, to make sure that commands are printed
                    before they are being executed.|})
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
  let auto_promote =
    Arg.(value
         & flag
         & info ["auto-promote"] ~docs
             ~doc:"Automatically promote files. This is similar to running
                   $(b,jbuilder promote) after the build.")
  in
  let force =
    Arg.(value
         & flag
         & info ["force"; "f"]
             ~doc:"Force actions associated to aliases to be re-executed even
                   if their dependencies haven't changed.")
  in
  let for_release = "for-release-of-packages" in
  let frop =
    Arg.(value
         & opt (some string) None
         & info ["p"; for_release] ~docs ~docv:"PACKAGES"
             ~doc:{|Shorthand for $(b,--root . --only-packages PACKAGE --promote ignore).
                    You must use this option in your $(i,<package>.opam) files, in order
                    to build only what's necessary when your project contains multiple
                    packages as well as getting reproducible builds.|})
  in
  let root_and_only_packages =
    let merge root only_packages release =
      let fail opt =
        `Error (true,
                sprintf
                  "Cannot use -p/--%s and %s simultaneously"
                  for_release opt)
      in
      match release, root, only_packages with
      | Some _, Some _, _ -> fail "--root"
      | Some _, _, Some _ -> fail "--only-packages"
      | Some pkgs, None, None ->
        `Ok (Some ".",
             Some pkgs,
             ["-p"; pkgs]
            )
      | None, _, _ ->
        `Ok (root,
             only_packages,
             List.concat
               [ dump_opt "--root" root
               ; dump_opt "--only-packages" only_packages
               ])
    in
    Term.(ret (const merge
               $ root
               $ only_packages
               $ frop))
  in
  let x =
    Arg.(value
         & opt (some string) None
         & info ["x"] ~docs
             ~doc:{|Cross-compile using this toolchain.|})
  in
  let diff_command =
    Arg.(value
         & opt (some string) None
         & info ["diff-command"] ~docs
             ~doc:"Shell command to use to diff files")
  in
  Term.(const make
        $ concurrency
        $ ddep_path
        $ dfindlib
        $ dbacktraces
        $ dev
        $ verbose
        $ no_buffer
        $ workspace_file
        $ diff_command
        $ auto_promote
        $ force
        $ root_and_only_packages
        $ x
       )

let installed_libraries =
  let doc = "Print out libraries installed on the system." in
  let go common na =
    set_common common ~targets:[];
    Future.Scheduler.go ~log:(Log.create ())
      (Context.create (Default [Native])  >>= fun ctxs ->
       let ctx = List.hd ctxs in
       let findlib = ctx.findlib in
       if na then begin
         let pkgs = Findlib.all_unavailable_packages findlib in
         let longest = List.longest_map pkgs ~f:(fun na -> na.package) in
         let ppf = Format.std_formatter in
         List.iter pkgs ~f:(fun (na : Findlib.Package_not_available.t) ->
           Format.fprintf ppf "%-*s -> %a@\n" longest na.package
             Findlib.Package_not_available.explain na.reason);
         Format.pp_print_flush ppf ();
         Future.return ()
       end else begin
         let pkgs = Findlib.all_packages findlib in
         let max_len = List.longest_map pkgs ~f:(fun p -> p.name) in
         List.iter pkgs ~f:(fun pkg ->
           let ver =
             match pkg.Findlib.version with
             | "" -> "n/a"
             | v  -> v
           in
           Printf.printf "%-*s (version: %s)\n" max_len pkg.name ver);
         Future.return ()
       end)
  in
  ( Term.(const go
          $ common
          $ Arg.(value
                 & flag
                 & info ["na"; "not-available"]
                     ~doc:"List libraries that are not available and explain why"))
  , Term.info "installed-libraries" ~doc
  )

let resolve_package_install setup pkg =
  match Main.package_install_file setup pkg with
  | Ok path -> path
  | Error () ->
    die "Unknown package %s!%s" pkg (hint pkg (String_map.keys setup.packages))

let target_hint (setup : Main.setup) path =
  assert (Path.is_local path);
  let sub_dir = Path.parent path in
  let candidates = Build_system.all_targets setup.build_system in
  let candidates =
    if Path.is_in_build_dir path then
      candidates
    else
      List.map candidates ~f:(fun path ->
        match Path.extract_build_context path with
        | None -> path
        | Some (_, path) -> path)
  in
  let candidates =
    (* Only suggest hints for the basename, otherwise it's slow when there are lots of
       files *)
    List.filter_map candidates ~f:(fun path ->
      if Path.parent path = sub_dir then
        Some (Path.to_string path)
      else
        None)
  in
  let candidates = String_set.of_list candidates |> String_set.elements in
  hint (Path.to_string path) candidates

let check_path contexts =
  let contexts = String_set.of_list (List.map contexts ~f:(fun c -> c.Context.name)) in
  fun path ->
    let internal path =
      die "This path is internal to jbuilder: %s" (Path.to_string_maybe_quoted path)
    in
    if Path.is_in_build_dir path then
      match Path.extract_build_context path with
      | None -> internal path
      | Some (name, _) ->
        if name = "" || name.[0] = '.' then internal path;
        if not (name = "install" || String_set.mem name contexts) then
          die "%s refers to unknown build context: %s%s"
            (Path.to_string_maybe_quoted path)
            name
            (hint name (String_set.elements contexts))

let resolve_targets ~log common (setup : Main.setup) user_targets =
  match user_targets with
  | [] -> []
  | _ ->
    let check_path = check_path setup.contexts in
    let targets =
      List.map user_targets ~f:(fun s ->
        if String.is_prefix s ~prefix:"@" then begin
          let s = String.sub s ~pos:1 ~len:(String.length s - 1) in
          let path = Path.relative Path.root (prefix_target common s) in
          check_path path;
          if Path.is_root path then
            die "@@ on the command line must be followed by a valid alias name"
          else if not (Path.is_local path) then
            die "@@ on the command line must be followed by a relative path"
          else
            Ok [Alias_rec path]
        end else begin
          let path = Path.relative Path.root (prefix_target common s) in
          check_path path;
          let can't_build path =
            Error (path, target_hint setup path);
          in
          if not (Path.is_local path) then
            Ok [File path]
          else if Path.is_in_build_dir path then begin
            if Build_system.is_target setup.build_system path then
              Ok [File path]
            else
              can't_build path
          end else
            match
              List.filter_map setup.contexts ~f:(fun ctx ->
                let path = Path.append ctx.Context.build_dir path in
                if Build_system.is_target setup.build_system path then
                  Some (File path)
                else
                  None)
            with
            | [] -> can't_build path
            | l  -> Ok l
        end
      )
    in
    if !Clflags.verbose then begin
      Log.info log "Actual targets:";
      let targets =
        List.concat_map targets ~f:(function
          | Ok targets -> targets
          | Error _ -> []) in
      List.iter targets ~f:(function
        | File path ->
          Log.info log @@ "- " ^ (Path.to_string path)
        | Alias_rec path ->
          Log.info log @@ "- recursive alias " ^
                          (Path.to_string_maybe_quoted path));
      flush stdout;
    end;
    targets

let resolve_targets_exn ~log common setup user_targets =
  resolve_targets ~log common setup user_targets
  |> List.concat_map ~f:(function
    | Error (path, hint) ->
      die "Don't know how to build %a%s" Path.pp path hint
    | Ok targets ->
      targets)

let build_targets =
  let doc = "Build the given targets, or all installable targets if none are given." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Targets starting with a $(b,@) are interpreted as aliases.|}
    ; `Blocks help_secs
    ]
  in
  let name_ = Arg.info [] ~docv:"TARGET" in
  let go common targets =
    set_common common ~targets;
    let log = Log.create () in
    Future.Scheduler.go ~log
      (Main.setup ~log common >>= fun setup ->
       let targets = resolve_targets_exn ~log common setup targets in
       do_build setup targets) in
  ( Term.(const go
          $ common
          $ Arg.(value & pos_all string ["@install"] name_))
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
    set_common common
      ~targets:(List.map dirs ~f:(function
        | "" | "." -> "@runtest"
        | dir when dir.[String.length dir - 1] = '/' -> sprintf "@%sruntest" dir
        | dir -> sprintf "@%s/runtest" dir));
    let log = Log.create () in
    Future.Scheduler.go ~log
      (Main.setup ~log common >>= fun setup ->
       let check_path = check_path setup.contexts in
       let targets =
         List.map dirs ~f:(fun dir ->
           let dir = Path.(relative root) (prefix_target common dir) in
           check_path dir;
           Alias_rec (Path.relative dir "runtest"))
       in
       do_build setup targets) in
  ( Term.(const go
          $ common
          $ Arg.(value & pos_all string ["."] name_))
  , Term.info "runtest" ~doc ~man)

let clean =
  let doc = "Clean the project." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Removes files added by jbuilder such as _build, <package>.install, and .merlin|}
    ; `Blocks help_secs
    ]
  in
  let go common =
    begin
      set_common common ~targets:[];
      Build_system.files_in_source_tree_to_delete ()
      |> List.iter ~f:Path.unlink_no_err;
      Path.(rm_rf (append root (of_string "_build")))
    end
  in
  ( Term.(const go $ common)
  , Term.info "clean" ~doc ~man)

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
    set_common common ~targets:[];
    let log = Log.create () in
    Future.Scheduler.go ~log
      (Main.setup ~log common ~filter_out_optional_stanzas_with_missing_deps:false
       >>= fun setup ->
       let targets = resolve_targets_exn ~log common setup targets in
       let request = request_of_targets setup targets in
       let failure =
         String_map.fold ~init:false
           (Build_system.all_lib_deps_by_context setup.build_system ~request)
           ~f:(fun ~key:context_name ~data:lib_deps acc ->
             let internals =
               Jbuild.Stanzas.lib_names
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
               else if String_map.for_all missing ~f:(fun _ kind -> kind = Build.Optional)
               then begin
                 Format.eprintf
                   "@{<error>Error@}: The following libraries are missing \
                    in the %s context:\n\
                    %s@."
                   context_name
                   (format_external_libs missing);
                 false
               end else begin
                 Format.eprintf
                   "@{<error>Error@}: The following libraries are missing \
                    in the %s context:\n\
                    %s\n\
                    Hint: try: opam install %s@."
                   context_name
                   (format_external_libs missing)
                   (String_map.bindings missing
                    |> List.filter_map ~f:(fun (name, kind) ->
                      match (kind : Build.lib_dep_kind) with
                      | Optional -> None
                      | Required -> Some (Findlib.root_package_name name))
                    |> String_set.of_list
                    |> String_set.elements
                    |> String.concat ~sep:" ");
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

let rules =
  let doc = "Dump internal rules." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Dump Jbuilder internal rules for the given targets.
           If no targets are given, dump all the internal rules.|}
    ; `P {|By default the output is a list of S-expressions,
           one S-expression per rule. Each S-expression is of the form:|}
    ; `Pre "  ((deps    (<dependencies>))\n\
           \   (targets (<targets>))\n\
           \   (context <context-name>)\n\
           \   (action  <action>))"
    ; `P {|$(b,<context-name>) is the context is which the action is executed.
           It is omitted if the action is independant from the context.|}
    ; `P {|$(b,<action>) is the action following the same syntax as user actions,
           as described in the manual.|}
    ; `Blocks help_secs
    ]
  in
  let go common out recursive makefile_syntax targets =
    set_common common ~targets;
    let log = Log.create () in
    Future.Scheduler.go ~log
      (Main.setup ~log common ~filter_out_optional_stanzas_with_missing_deps:false
       >>= fun setup ->
       let request =
         match targets with
         | [] -> Build.paths (Build_system.all_targets setup.build_system)
         | _  -> resolve_targets_exn ~log common setup targets |> request_of_targets setup
       in
       Build_system.build_rules setup.build_system ~request ~recursive >>= fun rules ->
       let print oc =
         let ppf = Format.formatter_of_out_channel oc in
         Sexp.prepare_formatter ppf;
         Format.pp_open_vbox ppf 0;
         if makefile_syntax then begin
           List.iter rules ~f:(fun (rule : Build_system.Rule.t) ->
             Format.fprintf ppf "@[<hov 2>@{<makefile-stuff>%a:%t@}@]@,@<0>\t@{<makefile-action>%a@}@,@,"
               (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf p ->
                  Format.pp_print_string ppf (Path.to_string p)))
               (Path.Set.elements rule.targets)
               (fun ppf ->
                  Path.Set.iter rule.deps ~f:(fun dep ->
                    Format.fprintf ppf "@ %s" (Path.to_string dep)))
               Sexp.pp_split_strings (Action.sexp_of_t rule.action))
         end else begin
           List.iter rules ~f:(fun (rule : Build_system.Rule.t) ->
             let sexp =
               let paths ps = Sexp.To_sexp.list Path.sexp_of_t (Path.Set.elements ps) in
               Sexp.To_sexp.record (
                 List.concat
                   [ [ "deps"   , paths rule.deps
                     ; "targets", paths rule.targets ]
                   ; (match rule.context with
                      | None -> []
                      | Some c -> ["context", Atom c.name])
                   ; [ "action" , Action.sexp_of_t rule.action ]
                   ])
             in
             Format.fprintf ppf "%a@," Sexp.pp_split_strings sexp)
         end;
         Format.pp_print_flush ppf ();
         Future.return ()
       in
       match out with
       | None -> print stdout
       | Some fn -> Io.with_file_out fn ~f:print)
  in
  ( Term.(const go
          $ common
          $ Arg.(value
                 & opt (some string) None
                 & info ["o"] ~docv:"FILE"
                     ~doc:"Output to a file instead of stdout.")
          $ Arg.(value
                 & flag
                 & info ["r"; "recursive"]
                     ~doc:"Print all rules needed to build the transitive dependencies of the given targets.")
          $ Arg.(value
                 & flag
                 & info ["m"; "makefile"]
                     ~doc:"Output the rules in Makefile syntax.")
          $ Arg.(value
                 & pos_all string []
                 & Arg.info [] ~docv:"TARGET"))
  , Term.info "rules" ~doc ~man)

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

let get_libdir context ~libdir_from_command_line =
  match libdir_from_command_line with
  | Some p -> Future.return (Some (Path.of_string p))
  | None -> Context.install_ocaml_libdir context

let install_uninstall ~what =
  let doc =
    sprintf "%s packages using opam-installer." (String.capitalize_ascii what)
  in
  let name_ = Arg.info [] ~docv:"PACKAGE" in
  let go common prefix_from_command_line libdir_from_command_line pkgs =
    set_common common ~targets:[];
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
       (match setup.contexts, prefix_from_command_line, libdir_from_command_line with
        | _ :: _ :: _, Some _, _ | _ :: _ :: _, _, Some _ ->
          die "Cannot specify --prefix or --libdir when installing \
               into multiple contexts!"
        | _ -> ());
       let module CMap = Map.Make(Context) in
       let install_files_by_context =
         CMap.of_alist_multi install_files |> CMap.bindings
       in
       Future.all_unit
         (List.map install_files_by_context ~f:(fun (context, install_files) ->
            get_prefix context ~from_command_line:prefix_from_command_line
            >>= fun prefix ->
            get_libdir context ~libdir_from_command_line
            >>= fun libdir ->
            Future.all_unit
              (List.map install_files ~f:(fun path ->
                 let purpose = Future.Build_job install_files in
                 Future.run ~purpose Strict (Path.to_string opam_installer)
                   ([ sprintf "-%c" what.[0]
                    ; Path.to_string path
                    ; "--prefix"
                    ; Path.to_string prefix
                    ] @
                    match libdir with
                    | None -> []
                    | Some p -> [ "--libdir"; Path.to_string p ]
                   ))))))
  in
  ( Term.(const go
          $ common
          $ Arg.(value
                 & opt (some dir) None
                 & info ["destdir"; "prefix"]
                     ~docv:"PREFIX"
                     ~doc:"Directory where files are copied. For instance binaries \
                           are copied into $(i,\\$prefix/bin), library files into \
                           $(i,\\$prefix/lib), etc... It defaults to the current opam \
                           prefix if opam is available and configured, otherwise it uses \
                           the same prefix as the ocaml compiler.")
          $ Arg.(value
                 & opt (some dir) None
                 & info ["libdir"]
                     ~docv:"PATH"
                     ~doc:"Directory where library files are copied, relative to \
                           $(b,prefix) or absolute. If $(b,--prefix) \
                           is specified the default is $(i,\\$prefix/lib), otherwise \
                           it is the output of $(b,ocamlfind printconf destdir)"
                )
          $ Arg.(value & pos_all string [] name_))
  , Term.info what ~doc ~man:help_secs)

let install   = install_uninstall ~what:"install"
let uninstall = install_uninstall ~what:"uninstall"

let context_arg ~doc =
  Arg.(value
       & opt string "default"
       & info ["context"] ~docv:"CONTEXT" ~doc)

let exec =
  let doc =
    "Execute a command in a similar environment as if installation was performed."
  in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|$(b,jbuilder exec -- COMMAND) should behave in the same way as if you
           do:|}
    ; `Pre "  \\$ jbuilder install\n\
           \  \\$ COMMAND"
    ; `P {|In particular if you run $(b,jbuilder exec ocaml), you will have
           access to the libraries defined in the workspace using your usual
           directives ($(b,#require) for instance)|}
    ; `P {|When a leading / is present in the command (absolute path), then the
           path is interpreted as an absolute path|}
    ; `P {|When a / is present at any other position (relative path), then the
           path is interpreted as relative to the build context + current
           working directory (or the value of $(b,--root) when ran outside of
           the project root)|}
    ; `Blocks help_secs
    ]
  in
  let go common context prog no_rebuild args =
    set_common common ~targets:[];
    let log = Log.create () in
    let setup = Future.Scheduler.go ~log (Main.setup ~log common) in
    let context = Main.find_context_exn setup ~name:context in
    let prog_where =
      match Filename.analyze_program_name prog with
      | Absolute ->
        `This_abs (Path.of_string prog)
      | In_path ->
        `Search prog
      | Relative_to_current_dir ->
        let prog = prefix_target common prog in
        `This_rel (Path.relative context.build_dir prog) in
    let targets = lazy (
      (match prog_where with
       | `Search p ->
         [Path.relative (Config.local_install_bin_dir ~context:context.name) p]
       | `This_rel p when Sys.win32 ->
         [p; Path.extend_basename p ~suffix:Bin.exe]
       | `This_rel p ->
         [p]
       | `This_abs p when Path.is_in_build_dir p ->
         [p]
       | `This_abs _ ->
         [])
      |> List.map ~f:Path.to_string
      |> resolve_targets ~log common setup
      |> List.concat_map ~f:(function
        | Ok targets -> targets
        | Error _ -> [])
    ) in
    let real_prog =
      if not no_rebuild then begin
        match Lazy.force targets with
        | [] -> ()
        | targets ->
          Future.Scheduler.go ~log (do_build setup targets);
          Build_system.finalize setup.build_system
      end;
      match prog_where with
      | `Search prog ->
        let path = Config.local_install_bin_dir ~context:context.name :: context.path in
        Bin.which prog ~path
      | `This_rel prog
      | `This_abs prog ->
        if Path.exists prog then
          Some prog
        else if not Sys.win32 then
          None
        else
          let prog = Path.extend_basename prog ~suffix:Bin.exe in
          Option.some_if (Path.exists prog) prog
    in
    match real_prog, no_rebuild with
    | None, true ->
      begin match Lazy.force targets with
      | [] ->
        Format.eprintf "@{<Error>Error@}: Program %S not found!@." prog;
        die ""
      | _::_ ->
        Format.eprintf "@{<Error>Error@}: Program %S isn't built yet \
                        you need to buid it first or remove the \
                        --no-build option.@." prog;
        die ""
      end
    | None, false ->
      Format.eprintf "@{<Error>Error@}: Program %S not found!@." prog;
      die ""
    | Some real_prog, _ ->
      let real_prog = Path.to_string real_prog     in
      let env       = Context.env_for_exec context in
      let argv      = Array.of_list (prog :: args) in
      restore_cwd_and_execve common real_prog argv env
  in
  ( Term.(const go
          $ common
          $ context_arg ~doc:{|Run the command in this build context.|}
          $ Arg.(required
                 & pos 0 (some string) None (Arg.info [] ~docv:"PROG"))
          $ Arg.(value & flag
                 & info ["no-build"]
                     ~doc:"don't rebuild target before executing")
          $ Arg.(value
                 & pos_right 0 string [] (Arg.info [] ~docv:"ARGS"))
         )
  , Term.info "exec" ~doc ~man)

let subst =
  let doc =
    "Substitute watermarks in source files."
  in
  let man =
    let var name desc =
      `Blocks [`Noblank; `P ("- $(b,%%" ^ name ^ "%%), " ^ desc) ]
    in
    let opam field =
      var ("PKG_" ^ String.uppercase_ascii field)
        ("contents of the $(b," ^ field ^ ":) field from the opam file")
    in
    [ `S "DESCRIPTION"
    ; `P {|Substitute $(b,%%ID%%) strings in source files, in a similar fashion to
           what topkg does in the default configuration.|}
    ; `P {|This command is only meant to be called when a user pins a package to
           its development version. Especially it replaces $(b,%%VERSION%%) strings
           by the version obtained from the vcs. Currently only git is supported and
           the version is obtained from the output of:|}
    ; `Pre {|  \$ git describe --always --dirty|}
    ; `P {|$(b,jbuilder subst) substitutes the variables that topkg substitutes with
           the defatult configuration:|}
    ; var "NAME" "the name of the package"
    ; var "VERSION" "output of $(b,git describe --always --dirty)"
    ; var "VERSION_NUM" "same as $(b,%%VERSION%%) but with a potential leading \
                         'v' or 'V' dropped"
    ; var "VCS_COMMIT_ID" "commit hash from the vcs"
    ; opam "maintainer"
    ; opam "authors"
    ; opam "homepage"
    ; opam "issues"
    ; opam "doc"
    ; opam "license"
    ; opam "repo"
    ; `P {|It is not possible to customize this list. If you wish to do so you need to
           configure topkg instead and use it to perform the substitution.|}
    ; `P {|Note that the expansion of $(b,%%NAME%%) is guessed using the following
           heuristic: if all the $(b,<package>.opam) files in the current directory are
           prefixed by the shortest package name, this prefix is used. Otherwise you must
           specify a name with the $(b,-n) command line option.|}
    ; `P {|In order to call $(b,jbuilder subst) when your package is pinned, add this line
           to the $(b,build:) field of your opam file:|}
    ; `Pre {|  ["jbuilder" "subst"] {pinned}|}
    ; `Blocks help_secs
    ]
  in
  let go common name =
    set_common common ~targets:[];
    Future.Scheduler.go (Watermarks.subst ?name ())
  in
  ( Term.(const go
          $ common
          $ Arg.(value
                 & opt (some string) None
                 & info ["n"; "name"] ~docv:"NAME"
                     ~doc:"Use this package name instead of detecting it.")
         )
  , Term.info "subst" ~doc ~man)

let utop =
  let doc = "Load library in utop" in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|$(b,jbuilder utop DIR) build and run utop toplevel with libraries defined in DIR|}
    ; `Blocks help_secs
    ] in
  let go common dir ctx_name args =
    let utop_target = dir |> Path.of_string |> Utop.utop_exe |> Path.to_string in
    set_common common ~targets:[utop_target];
    let log = Log.create () in
    let (build_system, context, utop_path) =
      (Main.setup ~log common >>= fun setup ->
       let context = Main.find_context_exn setup ~name:ctx_name in
       let setup = { setup with contexts = [context] } in
       let target =
         match resolve_targets_exn ~log common setup [utop_target] with
         | [] -> die "no libraries defined in %s" dir
         | [File target] -> target
         | [Alias_rec _] | _::_::_ -> assert false
       in
       do_build setup [File target] >>| fun () ->
       (setup.build_system, context, Path.to_string target)
      ) |> Future.Scheduler.go ~log in
    Build_system.finalize build_system;
    restore_cwd_and_execve common utop_path (Array.of_list (utop_path :: args))
      (Context.env_for_exec context)
  in
  let name_ = Arg.info [] ~docv:"PATH" in
  ( Term.(const go
          $ common
          $ Arg.(value & pos 0 dir "" name_)
          $ context_arg ~doc:{|Select context where to build/run utop.|}
          $ Arg.(value & pos_right 0 string [] (Arg.info [] ~docv:"ARGS")))
  , Term.info "utop" ~doc ~man )

let promote =
  let doc = "Promote files from the last run" in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Considering all actions of the form $(b,(diff a b)) that failed
           in the last run of jbuilder, $(b,jbuilder promote) does the following:

           If $(b,a) is present in the source tree but $(b,b) isn't, $(b,b) is
           copied over to $(b,a) in the source tree. The idea behind this is that
           you might use $(b,(diff file.expected file.generated)) and then call
           $(b,jbuilder promote) to promote the generated file.
         |}
    ; `Blocks help_secs
    ] in
  let go common =
    set_common common ~targets:[];
    Action.Promotion.promote_files_registered_in_last_run ()
  in
  ( Term.(const go
          $ common)
  , Term.info "promote" ~doc ~man )

let all =
  [ installed_libraries
  ; external_lib_deps
  ; build_targets
  ; runtest
  ; clean
  ; install
  ; uninstall
  ; exec
  ; subst
  ; rules
  ; utop
  ; promote
  ]

let default =
  let doc = "composable build system for OCaml" in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ common))
  , Term.info "jbuilder" ~doc ~version:"%%VERSION%%"
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
               long time and is used daily by hundreds of developers, which means
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
