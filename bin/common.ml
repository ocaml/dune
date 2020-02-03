open Stdune
module Config = Dune.Config
module Colors = Dune.Colors
module Clflags = Dune.Clflags
module Package = Dune.Package
module Profile = Dune.Profile
module Term = Cmdliner.Term
module Manpage = Cmdliner.Manpage

module Let_syntax = struct
  let ( let+ ) t f = Term.(const f $ t)

  let ( and+ ) a b = Term.(const (fun x y -> (x, y)) $ a $ b)
end

open Let_syntax

module Only_packages = struct
  type t =
    { names : Dune.Package.Name.Set.t
    ; command_line_option : string
    }
end

type t =
  { debug_dep_path : bool
  ; debug_findlib : bool
  ; debug_backtraces : bool
  ; profile : Profile.t option
  ; workspace_file : Arg.Path.t option
  ; root : Workspace_root.t
  ; target_prefix : string
  ; only_packages : Only_packages.t option
  ; capture_outputs : bool
  ; x : Dune.Context_name.t option
  ; diff_command : string option
  ; promote : Clflags.Promote.t option
  ; force : bool
  ; ignore_promoted_rules : bool
  ; build_dir : string
  ; no_print_directory : bool
  ; store_orig_src_dir : bool
  ; (* Original arguments for the external-lib-deps hint *)
    orig_args : string list
  ; config : Dune.Config.t
  ; default_target : Arg.Dep.t (* For build & runtest only *)
  ; watch : bool
  ; stats_trace_file : string option
  ; always_show_command_line : bool
  ; promote_install_files : bool
  }

let workspace_file t = t.workspace_file

let x t = t.x

let profile t = t.profile

let capture_outputs t = t.capture_outputs

let root t = t.root

let config t = t.config

let only_packages t = t.only_packages

let watch t = t.watch

let default_target t = t.default_target

let prefix_target common s = common.target_prefix ^ s

let set_dirs c =
  if c.root.dir <> Filename.current_dir_name then Sys.chdir c.root.dir;
  Path.set_root (Path.External.cwd ());
  Path.Build.set_build_dir (Path.Build.Kind.of_string c.build_dir)

let set_common_other ?log_file c ~targets =
  Console.init c.config.display;
  Log.init () ?file:log_file;
  Clflags.debug_dep_path := c.debug_dep_path;
  Clflags.debug_findlib := c.debug_findlib;
  Clflags.debug_backtraces := c.debug_backtraces;
  Clflags.capture_outputs := c.capture_outputs;
  Clflags.diff_command := c.diff_command;
  Clflags.promote := c.promote;
  Clflags.force := c.force;
  Clflags.watch := c.watch;
  Clflags.no_print_directory := c.no_print_directory;
  Clflags.store_orig_src_dir := c.store_orig_src_dir;
  Clflags.promote_install_files := c.promote_install_files;
  Clflags.external_lib_deps_hint :=
    List.concat
      [ [ "dune"; "external-lib-deps"; "--missing" ]
      ; c.orig_args
      ; List.map ~f:Arg.Dep.to_string_maybe_quoted targets
      ];
  Clflags.always_show_command_line := c.always_show_command_line;
  Clflags.ignore_promoted_rules := c.ignore_promoted_rules;
  Option.iter ~f:Dune.Stats.enable c.stats_trace_file

let set_common ?log_file c ~targets =
  set_dirs c;
  set_common_other ?log_file c ~targets

let footer =
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
  ; footer
  ]

type config_file =
  | No_config
  | Default
  | This of Path.t

let default_build_dir = "_build"

(* Allow options from term1 or exclusively options from term2. If the user
   passes options from both terms, an error is reported. *)
let one_of term1 term2 =
  Term.ret
  @@ let+ x, args1 = Term.with_used_args term1
     and+ y, args2 = Term.with_used_args term2 in
     match (args1, args2) with
     | _, [] -> `Ok x
     | [], _ -> `Ok y
     | arg1 :: _, arg2 :: _ ->
       `Error (true, sprintf "Cannot use %s and %s simultaneously" arg1 arg2)

let build_info =
  let+ build_info =
    Arg.(
      value & flag
      & info [ "build-info" ] ~docs:"OPTIONS" ~doc:"Show build information.")
  in
  if build_info then (
    let module B = Build_info.V1 in
    let pr fmt = Printf.printf (fmt ^^ "\n") in
    let ver_string v =
      match v with
      | None -> "n/a"
      | Some v -> B.Version.to_string v
    in
    pr "version: %s" (ver_string (B.version ()));
    let libs =
      B.Statically_linked_libraries.to_list ()
      |> List.map ~f:(fun lib ->
             ( B.Statically_linked_library.name lib
             , ver_string (B.Statically_linked_library.version lib) ))
      |> List.sort ~compare:Poly.compare
    in
    ( match libs with
    | [] -> ()
    | _ ->
      pr "statically linked libraries:";
      let longest = String.longest_map libs ~f:fst in
      List.iter libs ~f:(fun (name, v) -> pr "- %-*s %s" longest name v) );
    exit 0
  )

module Options_implied_by_dash_p = struct
  type t =
    { root : string option
    ; only_packages : Only_packages.t option
    ; ignore_promoted_rules : bool
    ; config_file : config_file
    ; profile : Profile.t option
    ; default_target : Arg.Dep.t
    ; always_show_command_line : bool
    ; promote_install_files : bool
    }

  let docs = copts_sect

  let config_file_term =
    let+ x =
      one_of
        (let+ fn =
           Arg.(
             value
             & opt (some path) None
             & info [ "config-file" ] ~docs ~docv:"FILE"
                 ~doc:"Load this configuration file instead of the default one.")
         in
         Option.map fn ~f:(fun fn -> This (Arg.Path.path fn)))
        (let+ x =
           Arg.(
             value & flag
             & info [ "no-config" ] ~docs
                 ~doc:"Do not load the configuration file")
         in
         Option.some_if x No_config)
    in
    Option.value x ~default:Default

  let packages =
    let parser s =
      Ok
        (Package.Name.Set.of_list
           (List.map ~f:Package.Name.of_string (String.split s ~on:',')))
    in
    let printer ppf set =
      Format.pp_print_string ppf
        (String.concat ~sep:","
           (Package.Name.Set.to_list set |> List.map ~f:Package.Name.to_string))
    in
    Arg.conv ~docv:"PACKAGES" (parser, printer)

  let options =
    let+ root =
      Arg.(
        value
        & opt (some dir) None
        & info [ "root" ] ~docs ~docv:"DIR"
            ~doc:
              {|Use this directory as workspace root instead of
                      guessing it. Note that this option doesn't change
                      the interpretation of targets given on the command
                      line. It is only intended for scripts.|})
    and+ only_packages =
      let+ names =
        Arg.(
          value
          & opt (some packages) None
          & info [ "only-packages" ] ~docs ~docv:"PACKAGES"
              ~doc:
                {|Ignore stanzas referring to a package that is not in
                      $(b,PACKAGES). $(b,PACKAGES) is a comma-separated list
                      of package names. Note that this has the same effect
                      as deleting the relevant stanzas from dune files.
                      It is mostly meant for releases. During development,
                      it is likely that what you want instead is to
                      build a particular $(b,<package>.install) target.|})
      in
      Option.map names ~f:(fun names ->
          { Only_packages.names; command_line_option = "only-packages" })
    and+ ignore_promoted_rules =
      Arg.(
        value & flag
        & info
            [ "ignore-promoted-rules" ]
            ~docs
            ~doc:
              "Ignore rules with (mode promote),\n\
              \                     except ones with (only ...). The variable\n\
              \                     %{ignoring_promoted_rules} in dune files \
               reflects\n\
              \                     whether this option was passed or not.")
    and+ config_file = config_file_term
    and+ default_target =
      Arg.(
        value
        & opt dep
            (Dep.alias ~dir:Stdune.Path.Local.root Dune.Alias.Name.default)
        & info [ "default-target" ] ~docs ~docv:"TARGET"
            ~doc:
              {|Set the default target that when none is specified to
                      $(b,dune build).|})
    and+ always_show_command_line =
      let doc =
        "Always show the full command lines of programs executed by dune"
      in
      Arg.(value & flag & info [ "always-show-command-line" ] ~docs ~doc)
    and+ promote_install_files =
      let doc =
        "Promote the generated <package>.install files to the source tree"
      in
      Arg.(value & flag & info [ "promote-install-files" ] ~docs ~doc)
    in
    { root
    ; only_packages
    ; ignore_promoted_rules
    ; config_file
    ; profile = None
    ; default_target
    ; always_show_command_line
    ; promote_install_files
    }

  let for_release = "for-release-of-packages"

  let dash_p =
    let+ pkgs, args =
      Term.with_used_args
        Arg.(
          value
          & opt (some packages) None
          & info [ "p"; for_release ] ~docs ~docv:"PACKAGES"
              ~doc:
                {|Shorthand for $(b,--root . --only-packages PACKAGE
                      --ignore-promoted-rules --no-config --profile release).
                      You must use this option in your $(i,<package>.opam)
                      files, in order to build only what's necessary when
                      your project contains multiple packages as well as
                      getting reproducible builds.|})
    in
    { root = Some "."
    ; only_packages =
        Option.map pkgs ~f:(fun names ->
            { Only_packages.names; command_line_option = List.hd args })
    ; ignore_promoted_rules = true
    ; config_file = No_config
    ; profile = Some Profile.Release
    ; default_target =
        Arg.Dep.alias_rec ~dir:Path.Local.root Dune.Alias.Name.install
    ; always_show_command_line = true
    ; promote_install_files = true
    }

  let term =
    let+ t = one_of options dash_p
    and+ profile =
      let doc =
        "Build profile. $(b,dev) if unspecified or $(b,release) if -p is set."
      in
      Arg.(
        value
        & opt (some profile) None
        & info [ "profile" ] ~docs
            ~env:(Arg.env_var ~doc "DUNE_PROFILE")
            ~doc:
              (Printf.sprintf
                 "Select the build profile, for instance $(b,dev) \
                  or$(b,release). The default is $(b,%s)."
                 (Profile.to_string Dune.Profile.default)))
    in
    match profile with
    | None -> t
    | Some _ -> { t with profile }
end

let display_term =
  one_of
    (let+ verbose =
       Arg.(
         value & flag
         & info [ "verbose" ] ~docs:copts_sect
             ~doc:"Same as $(b,--display verbose)")
     in
     Option.some_if verbose Config.Display.Verbose)
    Arg.(
      value
      & opt (some (enum Config.Display.all)) None
      & info [ "display" ] ~docs:copts_sect ~docv:"MODE"
          ~doc:
            {|Control the display mode of Dune.
         See $(b,dune-config\(5\)) for more details.|})

let config_of_file = function
  | No_config -> Config.default
  | This fname -> Config.load_config_file fname
  | Default ->
    if Config.inside_dune then
      Config.default
    else
      Config.load_user_config_file ()

let term =
  let docs = copts_sect in
  let+ concurrency =
    let arg =
      Arg.conv
        ( (fun s ->
            Result.map_error (Dune.Config.Concurrency.of_string s) ~f:(fun s ->
                `Msg s))
        , fun pp x ->
            Format.pp_print_string pp (Dune.Config.Concurrency.to_string x) )
    in
    Arg.(
      value
      & opt (some arg) None
      & info [ "j" ] ~docs ~docv:"JOBS"
          ~doc:{|Run no more than $(i,JOBS) commands simultaneously.|})
  and+ sandboxing_preference =
    let arg =
      Arg.conv
        ( (fun s ->
            Result.map_error (Dune.Sandbox_mode.of_string s) ~f:(fun s ->
                `Msg s))
        , fun pp x -> Format.pp_print_string pp (Dune.Sandbox_mode.to_string x)
        )
    in
    Arg.(
      value
      & opt (some arg) None
      & info [ "sandbox" ]
          ~env:
            (Arg.env_var
               ~doc:"Sandboxing mode to use by default. (see --sandbox)"
               "DUNE_SANDBOX")
          ~doc:
            (Printf.sprintf
               "Sandboxing mode to use by default. Some actions require a \
                certain sandboxing mode, so they will ignore this setting. The \
                allowed values are: %s."
               (String.concat ~sep:", "
                  (List.map Dune.Sandbox_mode.all
                     ~f:Dune.Sandbox_mode.to_string))))
  and+ debug_dep_path =
    Arg.(
      value & flag
      & info
          [ "debug-dependency-path" ]
          ~docs
          ~doc:
            {|In case of error, print the dependency path from
                    the targets on the command line to the rule that failed.
                  |})
  and+ debug_findlib =
    Arg.(
      value & flag
      & info [ "debug-findlib" ] ~docs ~doc:{|Debug the findlib sub-system.|})
  and+ debug_backtraces =
    Arg.(
      value & flag
      & info [ "debug-backtraces" ] ~docs
          ~doc:{|Always print exception backtraces.|})
  and+ terminal_persistence =
    Arg.(
      value
      & opt (some (enum Config.Terminal_persistence.all)) None
      & info [ "terminal-persistence" ] ~docs ~docv:"MODE"
          ~doc:
            {|
         Changes how the log of build results are displayed to the
         console between rebuilds while in --watch mode. |})
  and+ display = display_term
  and+ no_buffer =
    let doc =
      {|Do not buffer the output of commands executed by dune. By default dune
        buffers the output of subcommands, in order to prevent interleaving when
        multiple commands are executed in parallel. However, this can be an
        issue when debugging long running tests. With $(b,--no-buffer), commands
        have direct access to the terminal. Note that as a result their output
        won't be captured in the log file.

        You should use this option in conjunction with $(b,-j 1), to avoid
        interleaving. Additionally you should use $(b,--verbose) as well, to
        make sure that commands are printed before they are being executed.|}
    in
    Arg.(value & flag & info [ "no-buffer" ] ~docs ~docv:"DIR" ~doc)
  and+ workspace_file =
    let doc = "Use this specific workspace file instead of looking it up." in
    Arg.(
      value
      & opt (some path) None
      & info [ "workspace" ] ~docs ~docv:"FILE" ~doc
          ~env:(Arg.env_var ~doc "DUNE_WORKSPACE"))
  and+ promote =
    one_of
      (let+ auto =
         Arg.(
           value & flag
           & info [ "auto-promote" ] ~docs
               ~doc:
                 "Automatically promote files. This is similar to running\n\
                 \                   $(b,dune promote) after the build.")
       in
       Option.some_if auto Clflags.Promote.Automatically)
      (let+ disable =
         let doc = "Disable all promotion rules" in
         let env = Arg.env_var ~doc "DUNE_DISABLE_PROMOTION" in
         Arg.(value & flag & info [ "disable-promotion" ] ~docs ~env ~doc)
       in
       Option.some_if disable Clflags.Promote.Never)
  and+ force =
    Arg.(
      value & flag
      & info [ "force"; "f" ]
          ~doc:
            "Force actions associated to aliases to be re-executed even\n\
            \                   if their dependencies haven't changed.")
  and+ watch =
    Arg.(
      value & flag
      & info [ "watch"; "w" ]
          ~doc:
            "Instead of terminating build after completion, wait continuously \
             for file changes.")
  and+ { Options_implied_by_dash_p.root
       ; only_packages
       ; ignore_promoted_rules
       ; config_file
       ; profile
       ; default_target
       ; always_show_command_line
       ; promote_install_files
       } =
    Options_implied_by_dash_p.term
  and+ x =
    Arg.(
      value
      & opt (some Arg.context_name) None
      & info [ "x" ] ~docs ~doc:{|Cross-compile using this toolchain.|})
  and+ build_dir =
    let doc = "Specified build directory. _build if unspecified" in
    Arg.(
      value
      & opt (some string) None
      & info [ "build-dir" ] ~docs ~docv:"FILE"
          ~env:(Arg.env_var ~doc "DUNE_BUILD_DIR")
          ~doc)
  and+ diff_command =
    Arg.(
      value
      & opt (some string) None
      & info [ "diff-command" ] ~docs
          ~doc:
            "Shell command to use to diff files.\n\
            \                   Use - to disable printing the diff.")
  and+ stats_trace_file =
    Arg.(
      value
      & opt (some string) None
      & info [ "trace-file" ] ~docs ~docv:"FILE"
          ~doc:
            "Output trace data in catapult format\n\
            \                   (compatible with chrome://tracing)")
  and+ no_print_directory =
    Arg.(
      value & flag
      & info [ "no-print-directory" ] ~docs
          ~doc:"Suppress \"Entering directory\" messages")
  and+ store_orig_src_dir =
    let doc = "Store original source location in dune-package metadata" in
    Arg.(
      value & flag
      & info
          [ "store-orig-source-dir" ]
          ~docs
          ~env:(Arg.env_var ~doc "DUNE_STORE_ORIG_SOURCE_DIR")
          ~doc)
  and+ cache_mode =
    let doc = "Activate binary cache" in
    Arg.(
      value
      & opt (some (enum Config.Caching.Mode.all)) None
      & info [ "cache" ] ~docs ~env:(Arg.env_var ~doc "DUNE_CACHE") ~doc)
  and+ cache_transport =
    let doc = "Binary cache protocol" in
    Arg.(
      value
      & opt (some (enum Config.Caching.Transport.all)) None
      & info [ "cache-transport" ] ~docs
          ~env:(Arg.env_var ~doc "DUNE_CACHE_TRANSPORT")
          ~doc)
  and+ cache_duplication =
    let doc = "Binary cache duplication mode" in
    Arg.(
      value
      & opt (some (enum Config.Caching.Duplication.all)) None
      & info [ "cache-duplication" ] ~docs
          ~env:(Arg.env_var ~doc "DUNE_CACHE_DUPLICATION")
          ~doc)
  and+ cache_check_probability =
    let doc =
      "Probability cached rules are rerun to check for reproducibility"
    in
    Arg.(
      value
      & opt float Config.default.cache_check_probability
      & info
          [ "cache-check-probability" ]
          ~docs
          ~env:(Arg.env_var ~doc "DUNE_CACHE_CHECK_PROBABILITY")
          ~doc)
  and+ () = build_info in
  let build_dir = Option.value ~default:default_build_dir build_dir in
  let root = Workspace_root.create ~specified_by_user:root in
  let config = config_of_file config_file in
  let config =
    Config.merge config
      { display
      ; concurrency
      ; sandboxing_preference =
          Option.map sandboxing_preference ~f:(fun x -> [ x ])
      ; terminal_persistence
      ; cache_mode
      ; cache_transport
      ; cache_check_probability = Some cache_check_probability
      ; cache_duplication
      ; cache_trim_period = None
      ; cache_trim_size = None
      }
  in
  let config =
    Config.adapt_display config
      ~output_is_a_tty:(Lazy.force Ansi_color.stderr_supports_color)
  in
  { debug_dep_path
  ; debug_findlib
  ; debug_backtraces
  ; profile
  ; capture_outputs = not no_buffer
  ; workspace_file
  ; root
  ; orig_args = []
  ; target_prefix =
      String.concat ~sep:"" (List.map root.to_cwd ~f:(sprintf "%s/"))
  ; diff_command
  ; promote
  ; force
  ; ignore_promoted_rules
  ; only_packages
  ; x
  ; config
  ; build_dir
  ; no_print_directory
  ; store_orig_src_dir
  ; default_target
  ; watch
  ; stats_trace_file
  ; always_show_command_line
  ; promote_install_files
  }

let term =
  let+ t, orig_args = Term.with_used_args term in
  { t with orig_args }

let config_term =
  let+ config_file = Options_implied_by_dash_p.config_file_term in
  config_of_file config_file

let context_arg ~doc =
  Arg.(
    value
    & opt Arg.context_name Dune.Context_name.default
    & info [ "context" ] ~docv:"CONTEXT" ~doc)
