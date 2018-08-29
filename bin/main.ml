open! Stdune
open Dune
open Import
open Cmdliner
open Fiber.O

(* Things in src/ don't depend on cmdliner to speed up the
   bootstrap, so we set this reference here *)
let () = suggest_function := Cmdliner_suggest.value

module Arg = struct
  include Arg

  let package_name =
    Arg.conv ((fun p -> Ok (Package.Name.of_string p)), Package.Name.pp)

  module Path : sig
    type t
    val path : t -> Path.t
    val arg : t -> string

    val conv : t conv
  end = struct
    type t = string

    let path p = Path.of_filename_relative_to_initial_cwd p
    let arg s = s

    let conv = Arg.conv ((fun p -> Ok p), Format.pp_print_string)
  end

  let path = Path.conv

  [@@@ocaml.warning "-32"]
  let file = path
end

module Let_syntax = Cmdliner.Term

type common =
  { debug_dep_path        : bool
  ; debug_findlib         : bool
  ; debug_backtraces      : bool
  ; profile               : string option
  ; workspace_file        : Arg.Path.t option
  ; root                  : string
  ; target_prefix         : string
  ; only_packages         : Package.Name.Set.t option
  ; capture_outputs       : bool
  ; x                     : string option
  ; diff_command          : string option
  ; auto_promote          : bool
  ; force                 : bool
  ; ignore_promoted_rules : bool
  ; build_dir             : string
  ; (* Original arguments for the external-lib-deps hint *)
    orig_args             : string list
  ; config                : Config.t
  ; default_target        : string
  }

let prefix_target common s = common.target_prefix ^ s

let set_dirs c =
  if c.root <> Filename.current_dir_name then
    Sys.chdir c.root;
  Path.set_root (Path.External.cwd ());
  Path.set_build_dir (Path.Kind.of_string c.build_dir)

let set_common_other c ~targets =
  Clflags.debug_dep_path := c.debug_dep_path;
  Clflags.debug_findlib := c.debug_findlib;
  Clflags.debug_backtraces := c.debug_backtraces;
  Clflags.capture_outputs := c.capture_outputs;
  Clflags.diff_command := c.diff_command;
  Clflags.auto_promote := c.auto_promote;
  Clflags.force := c.force;
  Clflags.external_lib_deps_hint :=
    List.concat
      [ ["dune"; "external-lib-deps"; "--missing"]
      ; c.orig_args
      ; targets
      ]

let set_common c ~targets =
  set_dirs c;
  set_common_other c ~targets

let restore_cwd_and_execve common prog argv env =
  let env = Env.to_unix env in
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
  include Dune.Main

  let setup ~log ?external_lib_deps_mode common =
    setup
      ~log
      ?workspace_file:(Option.map ~f:Arg.Path.path common.workspace_file)
      ?only_packages:common.only_packages
      ?external_lib_deps_mode
      ?x:common.x
      ?profile:common.profile
      ~ignore_promoted_rules:common.ignore_promoted_rules
      ~capture_outputs:common.capture_outputs
      ()
end

module Log = struct
  include Dune.Log

  let create common =
    Log.create ~display:common.config.display ()
end

module Scheduler = struct
  include Dune.Scheduler

  let go ?log ~common fiber =
    let fiber =
      Main.set_concurrency ?log common.config
      >>= fun () ->
      fiber
    in
    Scheduler.go ?log ~config:common.config fiber
end

type target =
  | File      of Path.t
  | Alias     of Path.t
  | Alias_rec of Path.t

let parse_alias path ~contexts =
  let dir = Path.parent_exn path in
  let name = Path.basename path in
  match Path.extract_build_context dir with
  | None -> (contexts, dir, name)
  | Some ("install", _) ->
    die "Invalid alias: %s.\n\
         There are no aliases in %s."
      (Path.to_string_maybe_quoted Path.(relative build_dir "install"))
      (Path.to_string_maybe_quoted path)
  | Some (ctx, dir) -> ([ctx], dir, name)

let request_of_targets (setup : Main.setup) targets =
  let open Build.O in
  let contexts = List.map setup.contexts ~f:(fun c -> c.Context.name) in
  List.fold_left targets ~init:(Build.return ()) ~f:(fun acc target ->
    acc >>>
    match target with
    | File path -> Build.path path
    | Alias path ->
      let contexts, dir, name = parse_alias path ~contexts in
      Build_system.Alias.dep_multi_contexts ~dir ~name
        ~file_tree:setup.file_tree ~contexts
    | Alias_rec path ->
      let contexts, dir, name = parse_alias path ~contexts in
      Build_system.Alias.dep_rec_multi_contexts ~dir ~name
        ~file_tree:setup.file_tree ~contexts)

let do_build (setup : Main.setup) targets =
  Build_system.do_build setup.build_system
    ~request:(request_of_targets setup targets)

let find_root () =
  let cwd = Sys.getcwd () in
  let rec loop counter ~candidates ~to_cwd dir =
    let files = Sys.readdir dir |> Array.to_list |> String.Set.of_list in
    if String.Set.mem files Workspace.filename then
      cont counter ~candidates:((0, dir, to_cwd) :: candidates) dir ~to_cwd
    else if Which_program.t = Jbuilder && String.Set.exists files ~f:(fun fn ->
      String.is_prefix fn ~prefix:"jbuild-workspace") then
      cont counter ~candidates:((1, dir, to_cwd) :: candidates) dir ~to_cwd
    else if String.Set.mem files Dune_project.filename then
      cont counter ~candidates:((2, dir, to_cwd) :: candidates) dir ~to_cwd
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
    let (_, dir, to_cwd) =
      List.find_exn l ~f:(fun (prio, _, _) -> prio = lowest_priority)
    in
    (dir, to_cwd)

let common_footer =
  `Blocks
    [ `S "BUGS"
    ; `P "Check bug reports at https://github.com/ocaml/dune/issues"
    ]

let copts_sect = "COMMON OPTIONS"
let help_secs =
  [ `S copts_sect
  ; `P "These options are common to all commands."
  ; `S "MORE HELP"
  ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
  ; common_footer
  ]

type config_file =
  | No_config
  | Default
  | This of Path.t

let incompatible a b =
  `Error (true,
          sprintf
            "Cannot use %s and %s simultaneously"
            a b)

let common =
  let dump_opt name value =
    match value with
    | None -> []
    | Some s -> [name; s]
  in
  let docs = copts_sect in
  let%map concurrency =
    let arg =
      Arg.conv
        ((fun s ->
           Result.map_error (Config.Concurrency.of_string s)
             ~f:(fun s -> `Msg s)),
         fun pp x ->
           Format.pp_print_string pp (Config.Concurrency.to_string x))
    in
    Arg.(value
         & opt (some arg) None
         & info ["j"] ~docs ~docv:"JOBS"
             ~doc:{|Run no more than $(i,JOBS) commands simultaneously.|}
        )
  and debug_dep_path =
    Arg.(value
         & flag
         & info ["debug-dependency-path"] ~docs
             ~doc:{|In case of error, print the dependency path from
                    the targets on the command line to the rule that failed.
                  |})
  and debug_findlib =
    Arg.(value
         & flag
         & info ["debug-findlib"] ~docs
             ~doc:{|Debug the findlib sub-system.|})
  and debug_backtraces =
    Arg.(value
         & flag
         & info ["debug-backtraces"] ~docs
             ~doc:{|Always print exception backtraces.|})
  and display =
    Term.ret @@
    let%map verbose =
      Arg.(value
           & flag
           & info ["verbose"] ~docs
               ~doc:"Same as $(b,--display verbose)")
    and display =
      Arg.(value
           & opt (some (enum Config.Display.all)) None
           & info ["display"] ~docs ~docv:"MODE"
               ~doc:{|Control the display mode of Dune.
                      See $(b,dune-config\(5\)) for more details.|})
    in
    match verbose, display with
    | false , None   -> `Ok None
    | false , Some x -> `Ok (Some x)
    | true  , None   -> `Ok (Some Config.Display.Verbose)
    | true  , Some _ -> incompatible "--display" "--verbose"
  and no_buffer =
    Arg.(value
         & flag
         & info ["no-buffer"] ~docs ~docv:"DIR"
             ~doc:{|Do not buffer the output of commands executed by dune.
                    By default dune buffers the output of subcommands, in order
                    to prevent interleaving when multiple commands are executed
                    in parallel. However, this can be an issue when debugging
                    long running tests. With $(b,--no-buffer), commands have direct
                    access to the terminal. Note that as a result their output won't
                    be captured in the log file.

                    You should use this option in conjunction with $(b,-j 1),
                    to avoid interleaving. Additionally you should use
                    $(b,--verbose) as well, to make sure that commands are printed
                    before they are being executed.|})
  and workspace_file =
    Arg.(value
         & opt (some path) None
         & info ["workspace"] ~docs ~docv:"FILE"
             ~doc:"Use this specific workspace file instead of looking it up.")
  and auto_promote =
    Arg.(value
         & flag
         & info ["auto-promote"] ~docs
             ~doc:"Automatically promote files. This is similar to running
                   $(b,dune promote) after the build.")
  and force =
    Arg.(value
         & flag
         & info ["force"; "f"]
             ~doc:"Force actions associated to aliases to be re-executed even
                   if their dependencies haven't changed.")
  and root,
      only_packages,
      ignore_promoted_rules,
      config_file,
      profile,
      default_target,
      orig =
    let default_target_default =
      match Which_program.t with
      | Dune     -> "@@default"
      | Jbuilder -> "@install"
    in
    let for_release = "for-release-of-packages" in
    Term.ret @@
    let%map root =
      Arg.(value
           & opt (some dir) None
           & info ["root"] ~docs ~docv:"DIR"
               ~doc:{|Use this directory as workspace root instead of
                      guessing it. Note that this option doesn't change
                      the interpretation of targets given on the command
                      line. It is only intended for scripts.|})
    and only_packages =
      Arg.(value
           & opt (some string) None
           & info ["only-packages"] ~docs ~docv:"PACKAGES"
               ~doc:{|Ignore stanzas referring to a package that is not in
                      $(b,PACKAGES). $(b,PACKAGES) is a comma-separated list
                      of package names. Note that this has the same effect
                      as deleting the relevant stanzas from jbuild files.
                      It is mostly meant for releases. During development,
                      it is likely that what you want instead is to
                      build a particular $(b,<package>.install) target.|}
          )
    and ignore_promoted_rules =
      Arg.(value
           & flag
           & info ["ignore-promoted-rules"] ~docs
               ~doc:"Ignore rules with (mode promote)")
    and (config_file_opt, config_file) =
      Term.ret @@
      let%map config_file =
        Arg.(value
             & opt (some path) None
             & info ["config-file"] ~docs ~docv:"FILE"
                 ~doc:"Load this configuration file instead of \
                       the default one.")
      and no_config =
        Arg.(value
             & flag
             & info ["no-config"] ~docs
                 ~doc:"Do not load the configuration file")
      in
      match config_file, no_config with
      | None   , false -> `Ok (None, Default)
      | Some fn, false -> `Ok (Some "--config-file",
                               This (Arg.Path.path fn))
      | None   , true  -> `Ok (Some "--no-config"  , No_config)
      | Some _ , true  -> incompatible "--no-config" "--config-file"
    and profile =
      Term.ret @@
      let%map dev =
        Term.ret @@
        let%map dev =
          Arg.(value
               & flag
               & info ["dev"] ~docs
                   ~doc:{|Same as $(b,--profile dev)|})
        in
        match dev, Which_program.t with
        | false, (Dune | Jbuilder) -> `Ok false
        | true, Jbuilder -> `Ok true
        | true, Dune ->
          `Error
            (true, "--dev is no longer accepted as it is now the default.")
      and profile =
        Arg.(value
             & opt (some string) None
             & info ["profile"] ~docs
                 ~doc:
                   (sprintf
                      {|Select the build profile, for instance $(b,dev) or
                        $(b,release). The default is $(b,%s).|}
                      Config.default_build_profile))
      in
      match dev, profile with
      | false, x    -> `Ok x
      | true , None -> `Ok (Some "dev")
      | true , Some _ ->
        `Error (true,
                "Cannot use --dev and --profile simultaneously")
    and default_target =
      Arg.(value
           & opt (some string) None
           & info ["default-target"] ~docs ~docv:"TARGET"
               ~doc:(sprintf
                       {|Set the default target that when none is specified to
                         $(b,dune build). It defaults to %s.|}
                       default_target_default))
    and frop =
      Arg.(value
           & opt (some string) None
           & info ["p"; for_release] ~docs ~docv:"PACKAGES"
               ~doc:{|Shorthand for $(b,--root . --only-packages PACKAGE
                      --promote ignore --no-config --profile release).
                      You must use this option in your $(i,<package>.opam)
                      files, in order to build only what's necessary when
                      your project contains multiple packages as well as
                      getting reproducible builds.|})
    in
    let fail opt = incompatible ("-p/--" ^ for_release) opt in
    match frop, root, only_packages, ignore_promoted_rules,
          profile, default_target, config_file_opt with
    | Some _, Some _, _, _, _, _, _ -> fail "--root"
    | Some _, _, Some _, _, _, _, _ -> fail "--only-packages"
    | Some _, _, _, true  , _, _, _ -> fail "--ignore-promoted-rules"
    | Some _, _, _, _, Some _, _, _ -> fail "--profile"
    | Some _, _, _, _, _, Some s, _ -> fail s
    | Some _, _, _, _, _, _, Some _ -> fail "--default-target"
    | Some pkgs, None, None, false, None, None, None ->
      `Ok (Some ".",
           Some pkgs,
           true,
           No_config,
           Some "release",
           "@install",
           ["-p"; pkgs]
          )
    | None, _, _, _, _, _, _ ->
      `Ok (root,
           only_packages,
           ignore_promoted_rules,
           config_file,
           profile,
           Option.value default_target ~default:default_target_default,
           List.concat
             [ dump_opt "--root" root
             ; dump_opt "--only-packages" only_packages
             ; dump_opt "--profile" profile
             ; dump_opt "--default-target" default_target
             ; if ignore_promoted_rules then
                 ["--ignore-promoted-rules"]
               else
                 []
             ; (match config_file with
                | This fn   -> ["--config-file"; Path.to_string fn]
                | No_config -> ["--no-config"]
                | Default   -> [])
             ]
          )
  and x =
    Arg.(value
         & opt (some string) None
         & info ["x"] ~docs
             ~doc:{|Cross-compile using this toolchain.|})
  and build_dir =
    let doc = "Specified build directory. _build if unspecified" in
    Arg.(value
         & opt (some string) None
         & info ["build-dir"] ~docs ~docv:"FILE"
             ~env:(Arg.env_var ~doc "DUNE_BUILD_DIR")
             ~doc)
  and diff_command =
    Arg.(value
         & opt (some string) None
         & info ["diff-command"] ~docs
             ~doc:"Shell command to use to diff files")
  in
  let build_dir = Option.value ~default:"_build" build_dir in
  let root, to_cwd =
    match root with
    | Some dn -> (dn, [])
    | None ->
      if Config.inside_dune then
        (".", [])
      else
        find_root ()
  in
  let orig_args =
    List.concat
      [ dump_opt "--workspace" (Option.map ~f:Arg.Path.arg workspace_file)
      ; orig
      ]
  in
  let config =
    match config_file with
    | No_config  -> Config.default
    | This fname -> Config.load_config_file fname
    | Default    ->
      if Config.inside_dune then
        Config.default
      else
        Config.load_user_config_file ()
  in
  let config =
    Config.merge config
      { display
      ; concurrency
      }
  in
  let config =
    Config.adapt_display config
      ~output_is_a_tty:(Lazy.force Colors.stderr_supports_colors)
  in
  { debug_dep_path
  ; debug_findlib
  ; debug_backtraces
  ; profile
  ; capture_outputs = not no_buffer
  ; workspace_file
  ; root
  ; orig_args
  ; target_prefix = String.concat ~sep:"" (List.map to_cwd ~f:(sprintf "%s/"))
  ; diff_command
  ; auto_promote
  ; force
  ; ignore_promoted_rules
  ; only_packages =
      Option.map only_packages
        ~f:(fun s -> Package.Name.Set.of_list (
          List.map ~f:Package.Name.of_string (String.split s ~on:',')))
  ; x
  ; config
  ; build_dir
  ; default_target
  }

let installed_libraries =
  let doc = "Print out libraries installed on the system." in
  let term =
    let%map common = common
    and na =
      Arg.(value
           & flag
           & info ["na"; "not-available"]
               ~doc:"List libraries that are not available and explain why")
    in
    set_common common ~targets:[];
    let env = Main.setup_env ~capture_outputs:common.capture_outputs in
    Scheduler.go ~log:(Log.create common) ~common
      (Context.create
         (Default { loc = Loc.of_pos __POS__
                  ; targets = [Native]
                  ; profile = Config.default_build_profile
                  ; env     = None
                  })
         ~env
       >>= fun ctxs ->
       let ctx = List.hd ctxs in
       let findlib = ctx.findlib in
       if na then begin
         let pkgs = Findlib.all_unavailable_packages findlib in
         let longest =
           String.longest_map pkgs ~f:(fun (n, _) -> Lib_name.to_string n) in
         let ppf = Format.std_formatter in
         List.iter pkgs ~f:(fun (n, r) ->
           Format.fprintf ppf "%-*s -> %a@\n" longest (Lib_name.to_string n)
             Findlib.Unavailable_reason.pp r);
         Format.pp_print_flush ppf ();
         Fiber.return ()
       end else begin
         let pkgs = Findlib.all_packages findlib in
         let max_len = String.longest_map pkgs ~f:(fun n ->
           Findlib.Package.name n
           |> Lib_name.to_string) in
         List.iter pkgs ~f:(fun pkg ->
           let ver =
             Option.value (Findlib.Package.version pkg) ~default:"n/a"
           in
           Printf.printf "%-*s (version: %s)\n" max_len
             (Lib_name.to_string (Findlib.Package.name pkg)) ver);
         Fiber.return ()
       end)
  in
  (term, Term.info "installed-libraries" ~doc)

let resolve_package_install setup pkg =
  match Main.package_install_file setup pkg with
  | Ok path -> path
  | Error () ->
    let pkg = Package.Name.to_string pkg in
    die "Unknown package %s!%s" pkg
      (hint pkg
         (Package.Name.Map.keys setup.packages
          |> List.map ~f:Package.Name.to_string))

let target_hint (setup : Main.setup) path =
  assert (Path.is_managed path);
  let sub_dir = Option.value ~default:path (Path.parent path) in
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
      if Path.equal (Path.parent_exn path) sub_dir then
        Some (Path.to_string path)
      else
        None)
  in
  let candidates = String.Set.of_list candidates |> String.Set.to_list in
  hint (Path.to_string path) candidates

let check_path contexts =
  let contexts =
    String.Set.of_list (List.map contexts ~f:(fun c -> c.Context.name))
  in
  fun path ->
    let internal path =
      die "This path is internal to dune: %s"
        (Path.to_string_maybe_quoted path)
    in
    if Path.is_in_build_dir path then
      match Path.extract_build_context path with
      | None -> internal path
      | Some (name, _) ->
        if name = "" || name.[0] = '.' then internal path;
        if not (name = "install" || String.Set.mem contexts name) then
          die "%s refers to unknown build context: %s%s"
            (Path.to_string_maybe_quoted path)
            name
            (hint name (String.Set.to_list contexts))

type resolve_input =
  | Path of Path.t
  | String of string

let resolve_path path ~(setup : Main.setup) =
  check_path setup.contexts path;
  let can't_build path =
    Error (path, target_hint setup path);
  in
  if not (Path.is_managed path) then
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

let resolve_target common ~(setup : Main.setup) s =
  if String.is_prefix s ~prefix:"@" then begin
    let pos, is_rec =
      if String.length s >= 2 && s.[1] = '@' then
        (2, false)
      else
        (1, true)
    in
    let s = String.drop s pos in
    let path = Path.relative Path.root (prefix_target common s) in
    check_path setup.contexts path;
    if Path.is_root path then
      die "@@ on the command line must be followed by a valid alias name"
    else if not (Path.is_managed path) then
      die "@@ on the command line must be followed by a relative path"
    else
      Ok [if is_rec then Alias_rec path else Alias path]
  end else begin
    let path = Path.relative Path.root (prefix_target common s) in
    resolve_path path ~setup
  end

let log_targets ~log targets =
  List.iter targets ~f:(function
    | File path ->
      Log.info log @@ "- " ^ (Path.to_string path)
    | Alias path ->
      Log.info log @@ "- alias " ^
                      (Path.to_string_maybe_quoted path)
    | Alias_rec path ->
      Log.info log @@ "- recursive alias " ^
                      (Path.to_string_maybe_quoted path));
  flush stdout

let resolve_targets_mixed ~log common (setup : Main.setup) user_targets =
  match user_targets with
  | [] -> []
  | _ ->
    let targets =
      List.map user_targets ~f:(function
        | String s -> resolve_target common ~setup s
        | Path p -> resolve_path p ~setup) in
    if common.config.display = Verbose then begin
      Log.info log "Actual targets:";
      List.concat_map targets ~f:(function
        | Ok targets -> targets
        | Error _ -> [])
      |> log_targets ~log
    end;
    targets

let resolve_targets ~log common (setup : Main.setup) user_targets =
  List.map ~f:(fun s -> String s) user_targets
  |> resolve_targets_mixed ~log common setup

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
  let default_target =
    match Which_program.t with
    | Dune     -> "@@default"
    | Jbuilder -> "@install"
  in
  let term =
    let%map common = common
    and targets = Arg.(value & pos_all string [default_target] name_)
    in
    set_common common ~targets;
    let log = Log.create common in
    Scheduler.go ~log ~common
      (Main.setup ~log common >>= fun setup ->
       let targets = resolve_targets_exn ~log common setup targets in
       do_build setup targets)
  in
  (term, Term.info "build" ~doc ~man)

let runtest =
  let doc = "Run tests." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|This is a short-hand for calling:|}
    ; `Pre {|  dune build @runtest|}
    ; `Blocks help_secs
    ]
  in
  let name_ = Arg.info [] ~docv:"DIR" in
  let term =
    let%map common = common
    and dirs = Arg.(value & pos_all string ["."] name_)
    in
    set_common common
      ~targets:(List.map dirs ~f:(function
        | "" | "." -> "@runtest"
        | dir when dir.[String.length dir - 1] = '/' -> sprintf "@%sruntest" dir
        | dir -> sprintf "@%s/runtest" dir));
    let log = Log.create common in
    Scheduler.go ~log ~common
      (Main.setup ~log common >>= fun setup ->
       let check_path = check_path setup.contexts in
       let targets =
         List.map dirs ~f:(fun dir ->
           let dir = Path.(relative root) (prefix_target common dir) in
           check_path dir;
           Alias_rec (Path.relative dir "runtest"))
       in
       do_build setup targets)
  in
  (term, Term.info "runtest" ~doc ~man)

let clean =
  let doc = "Clean the project." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Removes files added by dune such as _build, <package>.install, and .merlin|}
    ; `Blocks help_secs
    ]
  in
  let term =
    let%map common = common
    in
    set_common common ~targets:[];
    Build_system.files_in_source_tree_to_delete ()
    |> Path.Set.iter ~f:Path.unlink_no_err;
    Path.rm_rf Path.build_dir
  in
  (term, Term.info "clean" ~doc ~man)

let format_external_libs libs =
  Lib_name.Map.to_list libs
  |> List.map ~f:(fun (name, kind) ->
    match (kind : Lib_deps_info.Kind.t) with
    | Optional -> sprintf "- %s (optional)" (Lib_name.to_string name)
    | Required -> sprintf "- %s" (Lib_name.to_string name))
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
  let term =
    let%map common = common
    and only_missing =
      Arg.(value
           & flag
           & info ["missing"]
               ~doc:{|Only print out missing dependencies|})
    and targets =
      Arg.(non_empty
           & pos_all string []
           & Arg.info [] ~docv:"TARGET")
    in
    set_common common ~targets:[];
    let log = Log.create common in
    Scheduler.go ~log ~common
      (Main.setup ~log common ~external_lib_deps_mode:true
       >>= fun setup ->
       let targets = resolve_targets_exn ~log common setup targets in
       let request = request_of_targets setup targets in
       let failure =
         String.Map.foldi ~init:false
           (Build_system.all_lib_deps_by_context setup.build_system ~request)
           ~f:(fun context_name lib_deps acc ->
             let internals =
               Super_context.internal_lib_names
                 (match String.Map.find setup.Main.scontexts context_name with
                  | None -> assert false
                  | Some x -> x)
             in
             let externals =
               Lib_name.Map.filteri lib_deps ~f:(fun name _ ->
                 not (Lib_name.Set.mem internals name))
             in
             if only_missing then begin
               let context =
                 List.find_exn setup.contexts ~f:(fun c -> c.name = context_name)
               in
               let missing =
                 Lib_name.Map.filteri externals ~f:(fun name _ ->
                   not (Findlib.available context.findlib name))
               in
               if Lib_name.Map.is_empty missing then
                 acc
               else if Lib_name.Map.for_alli missing
                         ~f:(fun _ kind -> kind = Lib_deps_info.Kind.Optional)
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
                   (Lib_name.Map.to_list missing
                    |> List.filter_map ~f:(fun (name, kind) ->
                      match (kind : Lib_deps_info.Kind.t) with
                      | Optional -> None
                      | Required -> Some (Lib_name.package_name name))
                    |> Package.Name.Set.of_list
                    |> Package.Name.Set.to_list
                    |> List.map ~f:Package.Name.to_string
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
       if failure then raise Already_reported;
       Fiber.return ())
  in
  (term, Term.info "external-lib-deps" ~doc ~man)

let rules =
  let doc = "Dump internal rules." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Dump Dune internal rules for the given targets.
           If no targets are given, dump all the internal rules.|}
    ; `P {|By default the output is a list of S-expressions,
           one S-expression per rule. Each S-expression is of the form:|}
    ; `Pre "  ((deps    (<dependencies>))\n\
           \   (targets (<targets>))\n\
           \   (context <context-name>)\n\
           \   (action  <action>))"
    ; `P {|$(b,<context-name>) is the context is which the action is executed.
           It is omitted if the action is independent from the context.|}
    ; `P {|$(b,<action>) is the action following the same syntax as user actions,
           as described in the manual.|}
    ; `Blocks help_secs
    ]
  in
  let term =
    let%map common = common
    and out =
      Arg.(value
           & opt (some string) None
           & info ["o"] ~docv:"FILE"
               ~doc:"Output to a file instead of stdout.")
    and recursive =
      Arg.(value
           & flag
           & info ["r"; "recursive"]
               ~doc:"Print all rules needed to build the transitive \
                     dependencies of the given targets.")
    and makefile_syntax =
      Arg.(value
           & flag
           & info ["m"; "makefile"]
               ~doc:"Output the rules in Makefile syntax.")
    and targets =
      Arg.(value
           & pos_all string []
           & Arg.info [] ~docv:"TARGET")
    in
    let out = Option.map ~f:Path.of_string out in
    set_common common ~targets;
    let log = Log.create common in
    Scheduler.go ~log ~common
      (Main.setup ~log common ~external_lib_deps_mode:true
       >>= fun setup ->
       let request =
         match targets with
         | [] -> Build.paths (Build_system.all_targets setup.build_system)
         | _  -> resolve_targets_exn ~log common setup targets |> request_of_targets setup
       in
       Build_system.build_rules setup.build_system ~request ~recursive >>= fun rules ->
       let sexp_of_action action =
         Action.for_shell action |> Action.For_shell.dgen
       in
       let print oc =
         let ppf = Format.formatter_of_out_channel oc in
         Dsexp.prepare_formatter ppf;
         Format.pp_open_vbox ppf 0;
         if makefile_syntax then begin
           List.iter rules ~f:(fun (rule : Build_system.Rule.t) ->
             Format.fprintf ppf "@[<hov 2>@{<makefile-stuff>%a:%t@}@]@,@<0>\t@{<makefile-action>%a@}@,@,"
               (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf p ->
                  Format.pp_print_string ppf (Path.to_string p)))
               (Path.Set.to_list rule.targets)
               (fun ppf ->
                  Path.Set.iter rule.deps ~f:(fun dep ->
                    Format.fprintf ppf "@ %s" (Path.to_string dep)))
               Dsexp.pp_split_strings (sexp_of_action rule.action))
         end else begin
           List.iter rules ~f:(fun (rule : Build_system.Rule.t) ->
             let sexp =
               let paths ps =
                 Dsexp.To_sexp.list Path_dsexp.dgen (Path.Set.to_list ps)
               in
               Dsexp.To_sexp.record (
                 List.concat
                   [ [ "deps"   , paths rule.deps
                     ; "targets", paths rule.targets ]
                   ; (match rule.context with
                      | None -> []
                      | Some c -> ["context",
                                   Dsexp.atom_or_quoted_string c.name])
                   ; [ "action" , sexp_of_action rule.action ]
                   ])
             in
             Format.fprintf ppf "%a@," Dsexp.pp_split_strings sexp)
         end;
         Format.pp_print_flush ppf ();
         Fiber.return ()
       in
       match out with
       | None -> print stdout
       | Some fn -> Io.with_file_out fn ~f:print)
  in
  (term, Term.info "rules" ~doc ~man)

let get_prefix context ~from_command_line =
  match from_command_line with
  | Some p -> Fiber.return (Path.of_string p)
  | None -> Context.install_prefix context

let get_libdir context ~prefix ~libdir_from_command_line =
  match libdir_from_command_line with
  | Some p -> Fiber.return (Some (Path.relative prefix p))
  | None -> Context.install_ocaml_libdir context

let print_unix_error f =
  try
    f ()
  with Unix.Unix_error (e, _, _) ->
    Format.eprintf "@{<error>Error@}: %s@."
      (Unix.error_message e)

let set_executable_bits   x = x lor  0o111
let clear_executable_bits x = x land (lnot 0o111)

let install_uninstall ~what =
  let doc =
    sprintf "%s packages using opam-installer." (String.capitalize what)
  in
  let name_ = Arg.info [] ~docv:"PACKAGE" in
  let term =
    let%map common = common
    and prefix_from_command_line =
      Arg.(value
           & opt (some dir) None
           & info ["destdir"; "prefix"]
               ~docv:"PREFIX"
               ~doc:"Directory where files are copied. For instance binaries \
                     are copied into $(i,\\$prefix/bin), library files into \
                     $(i,\\$prefix/lib), etc... It defaults to the current opam \
                     prefix if opam is available and configured, otherwise it uses \
                     the same prefix as the ocaml compiler.")
    and libdir_from_command_line =
      Arg.(value
           & opt (some dir) None
           & info ["libdir"]
               ~docv:"PATH"
               ~doc:"Directory where library files are copied, relative to \
                     $(b,prefix) or absolute. If $(b,--prefix) \
                     is specified the default is $(i,\\$prefix/lib), otherwise \
                     it is the output of $(b,ocamlfind printconf destdir)"
          )
    and pkgs =
      Arg.(value & pos_all package_name [] name_)
    in
    set_common common ~targets:[];
    let log = Log.create common in
    Scheduler.go ~log ~common
      (Main.setup ~log common >>= fun setup ->
       let pkgs =
         match pkgs with
         | [] -> Package.Name.Map.keys setup.packages
         | l  -> l
       in
       let install_files, missing_install_files =
         List.concat_map pkgs ~f:(fun pkg ->
           let fn = resolve_package_install setup pkg in
           List.map setup.contexts ~f:(fun ctx ->
             let fn = Path.append ctx.Context.build_dir fn in
             if Path.exists fn then
               Left (ctx, (pkg, fn))
             else
               Right fn))
         |> List.partition_map ~f:(fun x -> x)
       in
       if missing_install_files <> [] then begin
         die "The following <package>.install are missing:\n\
              %s\n\
              You need to run: dune build @install"
           (String.concat ~sep:"\n"
              (List.map missing_install_files
                 ~f:(fun p -> sprintf "- %s" (Path.to_string p))))
       end;
       (match
          setup.contexts, prefix_from_command_line, libdir_from_command_line
        with
        | _ :: _ :: _, Some _, _ | _ :: _ :: _, _, Some _ ->
          die "Cannot specify --prefix or --libdir when installing \
               into multiple contexts!"
        | _ -> ());
       let module CMap = Map.Make(Context) in
       let install_files_by_context =
         CMap.of_list_multi install_files |> CMap.to_list
       in
       Fiber.parallel_iter install_files_by_context
         ~f:(fun (context, install_files) ->
           get_prefix context ~from_command_line:prefix_from_command_line
           >>= fun prefix ->
           get_libdir context ~prefix ~libdir_from_command_line
           >>| fun libdir ->
           List.iter install_files ~f:(fun (package, path) ->
             let entries = Install.load_install_file path in
             let paths =
               Install.Section.Paths.make
                 ~package
                 ~destdir:prefix
                 ?libdir
                 ()
             in
             let files_deleted_in = ref Path.Set.empty in
             List.iter entries ~f:(fun { Install.Entry. src; dst; section } ->
               let src = src in
               let dst = Option.value dst ~default:(Path.basename src) in
               let dst =
                 Path.relative (Install.Section.Paths.get paths section) dst
               in
               let dir = Path.parent_exn dst in
               if what = "install" then begin
                 Printf.eprintf "Installing %s\n%!"
                   (Path.to_string_maybe_quoted dst);
                 Path.mkdir_p dir;
                 Io.copy_file () ~src ~dst
                   ~chmod:(
                     if Install.Section.should_set_executable_bit section then
                       set_executable_bits
                     else
                       clear_executable_bits)
               end else begin
                 if Path.exists dst then begin
                   Printf.eprintf "Deleting %s\n%!"
                     (Path.to_string_maybe_quoted dst);
                   print_unix_error (fun () -> Path.unlink dst)
                 end;
                 files_deleted_in := Path.Set.add !files_deleted_in dir;
               end;
               Path.Set.to_list !files_deleted_in
               (* This [List.rev] is to ensure we process children
                  directories before their parents *)
               |> List.rev
               |> List.iter ~f:(fun dir ->
                 if Path.exists dir then
                   match Path.readdir_unsorted dir with
                   | [] ->
                     Printf.eprintf "Deleting empty directory %s\n%!"
                       (Path.to_string_maybe_quoted dst);
                     print_unix_error (fun () -> Path.rmdir dir)
                   | _  -> ())))))
  in
  (term, Term.info what ~doc ~man:help_secs)

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
    ; `P {|$(b,dune exec -- COMMAND) should behave in the same way as if you
           do:|}
    ; `Pre "  \\$ dune install\n\
           \  \\$ COMMAND"
    ; `P {|In particular if you run $(b,dune exec ocaml), you will have
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
  let term =
    let%map common = common
    and context = context_arg ~doc:{|Run the command in this build context.|}
    and prog =
      Arg.(required
           & pos 0 (some string) None (Arg.info [] ~docv:"PROG"))
    and no_rebuild =
      Arg.(value & flag
           & info ["no-build"]
               ~doc:"don't rebuild target before executing")
    and args =
      Arg.(value
           & pos_right 0 string [] (Arg.info [] ~docv:"ARGS"))
    in
    set_common common ~targets:[prog];
    let log = Log.create common in
    let setup = Scheduler.go ~log ~common (Main.setup ~log common) in
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
      |> List.map ~f:(fun p -> Path p)
      |> resolve_targets_mixed ~log common setup
      |> List.concat_map ~f:(function
        | Ok targets -> targets
        | Error _ -> [])
    ) in
    let real_prog =
      if not no_rebuild then begin
        match Lazy.force targets with
        | [] -> ()
        | targets ->
          Scheduler.go ~log ~common (do_build setup targets);
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
        raise Already_reported
      | _::_ ->
        Format.eprintf "@{<Error>Error@}: Program %S isn't built yet \
                        you need to buid it first or remove the \
                        --no-build option.@." prog;
        raise Already_reported
      end
    | None, false ->
      Format.eprintf "@{<Error>Error@}: Program %S not found!@." prog;
      raise Already_reported
    | Some real_prog, _ ->
      let real_prog = Path.to_string real_prog     in
      let argv      = Array.of_list (prog :: args) in
      restore_cwd_and_execve common real_prog argv context.env
  in
  (term, Term.info "exec" ~doc ~man)

(** A string that is "%%VERSION%%" but not expanded by [dune subst] *)
let literal_version =
  "%%" ^ "VERSION%%"

let subst =
  let doc =
    "Substitute watermarks in source files."
  in
  let man =
    let var name desc =
      `Blocks [`Noblank; `P ("- $(b,%%" ^ name ^ "%%), " ^ desc) ]
    in
    let opam field =
      var ("PKG_" ^ String.uppercase field)
        ("contents of the $(b," ^ field ^ ":) field from the opam file")
    in
    [ `S "DESCRIPTION"
    ; `P {|Substitute $(b,%%ID%%) strings in source files, in a similar fashion to
           what topkg does in the default configuration.|}
    ; `P ({|This command is only meant to be called when a user pins a package to
            its development version. Especially it replaces $(b,|} ^ literal_version
          ^{|) strings by the version obtained from the vcs. Currently only git is
             supported and the version is obtained from the output of:|})
    ; `Pre {|  \$ git describe --always --dirty|}
    ; `P {|$(b,dune subst) substitutes the variables that topkg substitutes with
           the defatult configuration:|}
    ; var "NAME" "the name of the project (from the dune-project file)"
    ; var "VERSION" "output of $(b,git describe --always --dirty)"
    ; var "VERSION_NUM" ("same as $(b," ^ literal_version ^
                         ") but with a potential leading 'v' or 'V' dropped")
    ; var "VCS_COMMIT_ID" "commit hash from the vcs"
    ; opam "maintainer"
    ; opam "authors"
    ; opam "homepage"
    ; opam "issues"
    ; opam "doc"
    ; opam "license"
    ; opam "repo"
    ; `P {|In order to call $(b,dune subst) when your package is pinned, add this line
           to the $(b,build:) field of your opam file:|}
    ; `Pre {|  [dune "subst"] {pinned}|}
    ; `P {|Note that this command is meant to be called only from opam files and
           behaves a bit differently from other dune commands. In particular it
           doesn't try to detect the root and must be called from the root of
           the project.|}
    ; `Blocks help_secs
    ]
  in
  let term =
    match Which_program.t with
    | Jbuilder ->
      let%map common = common
      and name =
        Arg.(value
             & opt (some string) None
             & info ["n"; "name"] ~docv:"NAME"
                 ~doc:"Use this project name instead of detecting it.")
      in
      set_common common ~targets:[];
      Scheduler.go ~common (Watermarks.subst ?name ())
    | Dune ->
      let%map () = Term.const () in
      let config : Config.t =
        { display     = Quiet
        ; concurrency = Fixed 1
        }
      in
      Path.set_root (Path.External.cwd ());
      Dune.Scheduler.go ~config (Watermarks.subst ())
  in
  (term, Term.info "subst" ~doc ~man)

let utop =
  let doc = "Load library in utop" in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|$(b,dune utop DIR) build and run utop toplevel with libraries defined in DIR|}
    ; `Blocks help_secs
    ] in
  let term =
    let%map common = common
    and dir = Arg.(value & pos 0 string "" & Arg.info [] ~docv:"PATH")
    and ctx_name = context_arg ~doc:{|Select context where to build/run utop.|}
    and args = Arg.(value & pos_right 0 string [] (Arg.info [] ~docv:"ARGS"))
    in
    set_dirs common;
    let dir = Path.of_string dir in
    if not (Path.is_directory dir) then
      die "cannot find directory: %a" Path.pp dir;
    let utop_target = dir |> Utop.utop_exe |> Path.to_string in
    set_common_other common ~targets:[utop_target];
    let log = Log.create common in
    let (build_system, context, utop_path) =
      (Main.setup ~log common >>= fun setup ->
       let context = Main.find_context_exn setup ~name:ctx_name in
       let setup = { setup with contexts = [context] } in
       let target =
         match resolve_target common ~setup utop_target with
         | Error _ -> die "no library is defined in %a" Path.pp dir
         | Ok [File target] -> target
         | Ok _ -> assert false
       in
       do_build setup [File target] >>| fun () ->
       (setup.build_system, context, Path.to_string target)
      ) |> Scheduler.go ~log ~common in
    Build_system.finalize build_system;
    restore_cwd_and_execve common utop_path (Array.of_list (utop_path :: args))
      context.env
  in
  (term, Term.info "utop" ~doc ~man )

let promote =
  let doc = "Promote files from the last run" in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Considering all actions of the form $(b,(diff a b)) that failed
           in the last run of dune, $(b,dune promote) does the following:

           If $(b,a) is present in the source tree but $(b,b) isn't, $(b,b) is
           copied over to $(b,a) in the source tree. The idea behind this is that
           you might use $(b,(diff file.expected file.generated)) and then call
           $(b,dune promote) to promote the generated file.
         |}
    ; `Blocks help_secs
    ] in
  let term =
    let%map common = common
    and files =
      Arg.(value & pos_all Cmdliner.Arg.file [] & info [] ~docv:"FILE")
    in
    set_common common ~targets:[];
    (* We load and restore the digest cache as we need to clear the
       cache for promoted files, due to issues on OSX. *)
    Utils.Cached_digest.load ();
    Promotion.promote_files_registered_in_last_run
      (match files with
       | [] -> All
       | _ ->
         let files =
           List.map files
             ~f:(fun fn -> Path.of_string (prefix_target common fn))
         in
         let on_missing fn =
           Format.eprintf "@{<warning>Warning@}: Nothing to promote for %a.@."
             Path.pp fn
         in
         These (files, on_missing));
    Utils.Cached_digest.dump ()
  in
  (term, Term.info "promote" ~doc ~man )

let printenv =
  let doc = "Print the environment of a directory" in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|$(b,dune printenv DIR) prints the environment of a directory|}
    ; `Blocks help_secs
    ] in
  let term =
    let%map common = common
    and dir = Arg.(value & pos 0 dir "" & info [] ~docv:"PATH")
    in
    set_common common ~targets:[];
    let log = Log.create common in
    Scheduler.go ~log ~common (
      Main.setup ~log common >>= fun setup ->
      let dir = Path.of_string dir in
      check_path setup.contexts dir;
      let request =
        let dump sctx ~dir =
          let open Build.O in
          Super_context.dump_env sctx ~dir
          >>^ fun env ->
          ((Super_context.context sctx).name, env)
        in
        Build.all (
          match Path.extract_build_context dir with
          | Some (ctx, _) ->
            let sctx =
              String.Map.find setup.scontexts ctx |> Option.value_exn
            in
            [dump sctx ~dir]
          | None ->
            String.Map.values setup.scontexts
            |> List.map ~f:(fun sctx ->
              let dir =
                Path.append (Super_context.context sctx).build_dir dir
              in
              dump sctx ~dir)
        )
      in
      Build_system.do_build setup.build_system ~request
      >>| fun l ->
      let pp ppf = Format.fprintf ppf "@[<v1>(@,@[<v>%a@]@]@,)"
                     (Format.pp_print_list (Dsexp.pp Dune)) in
      match l with
      | [(_, env)] ->
        Format.printf "%a@." pp env
      | l ->
        List.iter l ~f:(fun (name, env) ->
          Format.printf "@[<v2>Environment for context %s:@,%a@]@." name pp env)
    )
  in
  (term, Term.info "printenv" ~doc ~man )

let fmt =
  let doc = "Format dune files" in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|$(b,dune unstable-fmt) reads a dune file and outputs a formatted
           version. This feature is unstable, and its interface or behaviour
           might change.
         |}
    ] in
  let term =
    let%map path_opt =
      let docv = "FILE" in
      let doc = "Path to the dune file to parse." in
      Arg.(value & pos 0 (some path) None & info [] ~docv ~doc)
    and inplace =
      let doc = "Modify the file in place" in
      Arg.(value & flag & info ["inplace"] ~doc)
    in
    if true then
      let (input, output) =
        match path_opt, inplace with
        | None, false ->
          (None, None)
        | Some path, true ->
          let path = Arg.Path.path path in
          (Some path, Some path)
        | Some path, false ->
          (Some (Arg.Path.path path), None)
        | None, true ->
          die "--inplace requires a file name"
      in
      Dune_fmt.format_file ~input ~output
    else
      die "This command is unstable. Please pass --unstable to use it nonetheless."
  in
  (term, Term.info "unstable-fmt" ~doc ~man )

module Help = struct
  let config =
    ("dune-config", 5, "", "Dune", "Dune manual"),
    [ `S Manpage.s_synopsis
    ; `Pre "~/.config/dune/config"
    ; `S Manpage.s_description
    ; `P {|Unless $(b,--no-config) or $(b,-p) is passed, Dune will read a
           configuration file from the user home directory. This file is used
           to control various aspects of the behavior of Dune.|}
    ; `P {|The configuration file is normally $(b,~/.config/dune/config) on
           Unix systems and $(b,Local Settings/dune/config) in the User home
           directory on Windows. However, it is possible to specify an
           alternative configuration file with the $(b,--config-file) option.|}
    ; `P {|The first line of the file must be of the form (lang dune X.Y)
           where X.Y is the version of the dune language used in the file.|}
    ; `P {|The rest of the file must be written in S-expression syntax and be
           composed of a list of stanzas. The following sections describe
           the stanzas available.|}
    ; `S "DISPLAY MODES"
    ; `P {|Syntax: $(b,\(display MODE\))|}
    ; `P {|This stanza controls how Dune reports what it is doing to the user.
           This parameter can also be set from the command line via $(b,--display MODE).
           The following display modes are available:|}
    ; `Blocks
        (List.map ~f:(fun (x, desc) -> `I (sprintf "$(b,%s)" x, desc))
           [ "progress",
             {|This is the default, Dune shows and update a
               status line as build goals are being completed.|}
           ; "quiet",
             {|Only display errors.|}
           ; "short",
             {|Print one line per command being executed, with the
               binary name on the left and the reason it is being executed for
               on the right.|}
           ; "verbose",
             {|Print the full command lines of programs being
               executed by Dune, with some colors to help differentiate
               programs.|}
           ])
    ; `P {|Note that when the selected display mode is $(b,progress) and the
           output is not a terminal then the $(b,quiet) mode is selected
           instead. This rule doesn't apply when running Dune inside Emacs.
           Dune detects whether it is executed from inside Emacs or not by
           looking at the environment variable $(b,INSIDE_EMACS) that is set by
           Emacs. If you want the same behavior with another editor, you can set
           this variable. If your editor already sets another variable,
           please open a ticket on the ocaml/dune github project so that we can
           add support for it.|}
    ; `S "JOBS"
    ; `P {|Syntax: $(b,\(jobs NUMBER\))|}
    ; `P {|Set the maximum number of jobs Dune might run in parallel.
           This can also be set from the command line via $(b,-j NUMBER).|}
    ; `P {|The default for this value is 4.|}
    ; common_footer
    ]

  type what =
    | Man of Manpage.t
    | List_topics

  let commands =
    [ "config", Man config
    ; "topics", List_topics
    ]

  let help =
    let doc = "Additional Dune help" in
    let man =
      [ `S "DESCRIPTION"
      ; `P {|$(b,dune help TOPIC) provides additional help on the given topic.
             The following topics are available:|}
      ; `Blocks (List.concat_map commands ~f:(fun (s, what) ->
          match what with
          | List_topics -> []
          | Man ((title, _, _, _, _), _) -> [`I (sprintf "$(b,%s)" s, title)]))
      ; common_footer
      ]
    in
    let term =
      Term.ret @@
      let%map man_format = Arg.man_format
      and what =
        Arg.(value
             & pos 0 (some (enum commands)) None
             & info [] ~docv:"TOPIC")
      in
      match what with
      | None ->
        `Help (man_format, Some "help")
      | Some (Man man_page) ->
        Format.printf "%a@?" (Manpage.print man_format) man_page;
        `Ok ()
      | Some List_topics ->
        List.filter_map commands ~f:(fun (s, what) ->
          match what with
          | List_topics -> None
          | _ -> Some s)
        |> List.sort ~compare:String.compare
        |> String.concat ~sep:"\n"
        |> print_endline;
        `Ok ()
    in
    (term, Term.info "help" ~doc ~man)
end

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
  ; printenv
  ; Help.help
  ; fmt
  ]

let default =
  let doc = "composable build system for OCaml" in
  let term =
    Term.ret @@
    let%map _ = common in
    `Help (`Pager, None)
  in
  (term,
   Term.info "dune" ~doc ~version:"%%VERSION%%"
     ~man:
       [ `S "DESCRIPTION"
       ; `P {|Dune is a build system designed for OCaml projects only. It
              focuses on providing the user with a consistent experience and takes
              care of most of the low-level details of OCaml compilation. All you
              have to do is provide a description of your project and Dune will
              do the rest.
            |}
       ; `P {|The scheme it implements is inspired from the one used inside Jane
              Street and adapted to the open source world. It has matured over a
              long time and is used daily by hundreds of developers, which means
              that it is highly tested and productive.
            |}
       ; `Blocks help_secs
       ])

let main () =
  Colors.setup_err_formatter_colors ();
  try
    match Term.eval_choice default all ~catch:false with
    | `Error _ -> exit 1
    | _ -> exit 0
  with
  | Fiber.Never -> exit 1
  | exn ->
    Report_error.report exn;
    exit 1
