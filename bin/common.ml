open Stdune
module Config = Dune_util.Config
module Colors = Dune_rules.Colors
module Clflags = Dune_engine.Clflags
module Graph = Dune_graph.Graph
module Package = Dune_engine.Package
module Profile = Dune_rules.Profile
module Term = Cmdliner.Term
module Manpage = Cmdliner.Manpage
module Only_packages = Dune_rules.Only_packages

module Let_syntax = struct
  let ( let+ ) t f = Term.(const f $ t)

  let ( and+ ) a b = Term.(const (fun x y -> (x, y)) $ a $ b)
end

open Let_syntax

type t =
  { debug_dep_path : bool
  ; debug_findlib : bool
  ; debug_backtraces : bool
  ; debug_artifact_substitution : bool
  ; debug_digests : bool
  ; wait_for_filesystem_clock : bool
  ; root : Workspace_root.t
  ; only_packages : Only_packages.Clflags.t
  ; capture_outputs : bool
  ; diff_command : string option
  ; promote : Clflags.Promote.t option
  ; force : bool
  ; ignore_promoted_rules : bool
  ; build_dir : string
  ; no_print_directory : bool
  ; store_orig_src_dir : bool
  ; rpc : Dune_rpc_impl.Server.t Lazy.t
  ; default_target : Arg.Dep.t (* For build & runtest only *)
  ; watch : Watch_mode_config.t
  ; print_metrics : bool
  ; dump_memo_graph_file : string option
  ; dump_memo_graph_format : Graph.File_format.t
  ; dump_memo_graph_with_timing : bool
  ; always_show_command_line : bool
  ; promote_install_files : bool
  ; stats : Dune_stats.t option
  ; file_watcher : Dune_engine.Scheduler.Run.file_watcher
  ; workspace_config : Dune_rules.Workspace.Clflags.t
  ; cache_debug_flags : Dune_engine.Cache_debug_flags.t
  ; report_errors_config : Dune_engine.Report_errors_config.t
  ; require_dune_project_file : bool
  ; insignificant_changes : [ `React | `Ignore ]
  }

let capture_outputs t = t.capture_outputs

let root t = t.root

let watch t = t.watch

let print_metrics t = t.print_metrics

let dump_memo_graph_file t = t.dump_memo_graph_file

let dump_memo_graph_format t = t.dump_memo_graph_format

let dump_memo_graph_with_timing t = t.dump_memo_graph_with_timing

let file_watcher t = t.file_watcher

let default_target t = t.default_target

let prefix_target t s = t.root.reach_from_root_prefix ^ s

let rpc t = Lazy.force t.rpc

let stats t = t.stats

let insignificant_changes t = t.insignificant_changes

let set_print_directory t b = { t with no_print_directory = not b }

let set_promote t v = { t with promote = Some v }

(* To avoid needless recompilations under Windows, where the case of
   [Sys.getcwd] can vary between different invocations of [dune], normalize to
   lowercase. *)
let normalize_path path =
  if Sys.win32 then
    let src = Path.External.to_string path in
    let is_letter = function
      | 'a' .. 'z' | 'A' .. 'Z' -> true
      | _ -> false
    in
    if String.length src >= 2 && is_letter src.[0] && src.[1] = ':' then (
      let dst = Bytes.create (String.length src) in
      Bytes.set dst 0 (Char.uppercase_ascii src.[0]);
      Bytes.blit_string ~src ~src_pos:1 ~dst ~dst_pos:1
        ~len:(String.length src - 1);
      Path.External.of_string (Bytes.unsafe_to_string dst))
    else path
  else path

let print_entering_message c =
  let cwd = Path.to_absolute_filename Path.root in
  if cwd <> Fpath.initial_cwd && not c.no_print_directory then
    (* Editors such as Emacs parse the output of the build system and interpret
       filenames in error messages relative to where the build system was
       started.

       If the build system changes directory, the editor will not be able to
       correctly locate files. However, such editors also understand messages of
       the form "Entering directory '<dir>'" that the "make" command prints.

       This is why Dune also prints such a message; this way people running Dune
       through such an editor will be able to use the "jump to error" feature of
       their editor. *)
    let dir =
      match Config.inside_dune with
      | false -> cwd
      | true -> (
        let descendant_simple p ~of_ =
          match String.drop_prefix p ~prefix:of_ with
          | None | Some "" -> None
          | Some s -> Some (String.drop s 1)
        in
        match descendant_simple cwd ~of_:Fpath.initial_cwd with
        | Some s -> s
        | None -> (
          match descendant_simple Fpath.initial_cwd ~of_:cwd with
          | None -> cwd
          | Some s ->
            let rec loop acc dir =
              if dir = Filename.current_dir_name then acc
              else loop (Filename.concat acc "..") (Filename.dirname dir)
            in
            loop ".." (Filename.dirname s)))
    in
    Console.print [ Pp.verbatim (sprintf "Entering directory '%s'" dir) ]

let init ?log_file c =
  if c.root.dir <> Filename.current_dir_name then Sys.chdir c.root.dir;
  Path.set_root (normalize_path (Path.External.cwd ()));
  Path.Build.set_build_dir (Path.Outside_build_dir.of_string c.build_dir);
  (* We need to print this before reading the workspace file, so that the editor
     can interpret errors in the workspace file. *)
  print_entering_message c;
  Dune_rules.Workspace.Clflags.set c.workspace_config;
  let config =
    (* Here we make the assumption that this computation doesn't yield. *)
    Fiber.run
      (Memo.run (Dune_rules.Workspace.workspace_config ()))
      ~iter:(fun () -> assert false)
  in
  let config =
    Dune_config.adapt_display config
      ~output_is_a_tty:(Lazy.force Ansi_color.stderr_supports_color)
  in
  Dune_config.init config;
  Dune_util.Log.init () ?file:log_file;
  Dune_engine.Execution_parameters.init
    (let open Memo.O in
    let+ w = Dune_rules.Workspace.workspace () in
    Dune_engine.Execution_parameters.builtin_default
    |> Dune_rules.Workspace.update_execution_parameters w);
  Dune_rules.Global.init ~capture_outputs:c.capture_outputs;
  let cache_config =
    match config.cache_enabled with
    | Disabled -> Dune_cache.Config.Disabled
    | Enabled ->
      Enabled
        { storage_mode =
            Option.value config.cache_storage_mode ~default:Hardlink
        ; reproducibility_check = config.cache_reproducibility_check
        }
  in
  Dune_util.Log.info
    [ Pp.textf "Shared cache: %s"
        (Dune_config.Cache.Enabled.to_string config.cache_enabled)
    ];
  Dune_rules.Main.init ~stats:c.stats
    ~sandboxing_preference:config.sandboxing_preference ~cache_config
    ~cache_debug_flags:c.cache_debug_flags;
  Only_packages.Clflags.set c.only_packages;
  Dune_util.Report_error.print_memo_stacks := c.debug_dep_path;
  Clflags.report_errors_config := c.report_errors_config;
  Clflags.debug_findlib := c.debug_findlib;
  Clflags.debug_backtraces c.debug_backtraces;
  Clflags.debug_artifact_substitution := c.debug_artifact_substitution;
  Clflags.debug_digests := c.debug_digests;
  Clflags.debug_fs_cache := c.cache_debug_flags.fs_cache;
  Clflags.wait_for_filesystem_clock := c.wait_for_filesystem_clock;
  Clflags.capture_outputs := c.capture_outputs;
  Clflags.diff_command := c.diff_command;
  Clflags.promote := c.promote;
  Clflags.force := c.force;
  Clflags.no_print_directory := c.no_print_directory;
  Clflags.store_orig_src_dir := c.store_orig_src_dir;
  Clflags.promote_install_files := c.promote_install_files;
  Clflags.always_show_command_line := c.always_show_command_line;
  Clflags.ignore_promoted_rules := c.ignore_promoted_rules;
  Clflags.on_missing_dune_project_file :=
    if c.require_dune_project_file then Error else Warn;
  Dune_util.Log.info
    [ Pp.textf "Workspace root: %s"
        (Path.to_absolute_filename Path.root |> String.maybe_quoted)
    ];
  config

let footer =
  `Blocks
    [ `S "BUGS"
    ; `P "Check bug reports at https://github.com/ocaml/dune/issues"
    ]

let copts_sect = "COMMON OPTIONS"

let debug_backtraces =
  Arg.(
    value & flag
    & info [ "debug-backtraces" ] ~docs:copts_sect
        ~doc:{|Always print exception backtraces.|})

let examples = function
  | [] -> `Blocks []
  | _ :: _ as examples ->
    let block_of_example index (intro, ex) =
      let prose = `I (Int.to_string (index + 1) ^ ".", String.trim intro ^ ":")
      and code_lines =
        ex |> String.trim |> String.split_lines
        |> List.concat_map ~f:(fun codeline ->
               [ `Noblank; `Pre ("      " ^ codeline) ])
        (* suppress initial blank *)
        |> List.tl
      in
      `Blocks (prose :: code_lines)
    in
    let example_blocks = examples |> List.mapi ~f:block_of_example in
    `Blocks (`S Cmdliner.Manpage.s_examples :: example_blocks)

(* Short reminders for the most used and useful commands *)
let command_synopsis commands =
  let format_command c acc =
    `Noblank :: `P (Printf.sprintf "$(b,dune %s)" c) :: acc
  in
  [ `S "SYNOPSIS"
  ; `Blocks (List.fold_right ~init:[] ~f:format_command commands)
  ]

let help_secs =
  [ `S copts_sect
  ; `P "These options are common to all commands."
  ; `S "MORE HELP"
  ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
  ; footer
  ]

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
      |> List.sort ~compare:(Tuple.T2.compare String.compare String.compare)
    in
    (match libs with
    | [] -> ()
    | _ ->
      pr "statically linked libraries:";
      let longest = String.longest_map libs ~f:fst in
      List.iter libs ~f:(fun (name, v) -> pr "- %-*s %s" longest name v));
    exit 0)

module Options_implied_by_dash_p = struct
  type t =
    { root : string option
    ; only_packages : Only_packages.Clflags.t
    ; ignore_promoted_rules : bool
    ; config_from_config_file : Dune_config.Partial.t
    ; profile : Profile.t option
    ; default_target : Arg.Dep.t
    ; always_show_command_line : bool
    ; promote_install_files : bool
    ; require_dune_project_file : bool
    }

  let docs = copts_sect

  type config_file =
    | No_config
    | Default
    | This of Path.t

  let config_term =
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
    match Option.value x ~default:Default with
    | No_config -> Dune_config.Partial.empty
    | This fname -> Dune_config.load_config_file fname
    | Default ->
      if Dune_util.Config.inside_dune then Dune_config.Partial.empty
      else Dune_config.load_user_config_file ()

  let packages =
    let parser s =
      let parse_one s =
        match Package.Name.of_string_opt s with
        | Some x -> Ok x
        | None ->
          ksprintf (fun s -> Error (`Msg s)) "Invalid package name: %S" s
      in
      String.split s ~on:',' |> List.map ~f:parse_one |> Result.List.all
      |> Result.map ~f:Package.Name.Set.of_list
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
              "Use this directory as workspace root instead of guessing it. \
               Note that this option doesn't change the interpretation of \
               targets given on the command line. It is only intended for \
               scripts.")
    and+ ignore_promoted_rules =
      Arg.(
        value & flag
        & info
            [ "ignore-promoted-rules" ]
            ~docs
            ~doc:
              "Ignore rules with (mode promote), except ones with (only ...). \
               The variable %{ignoring_promoted_rules} in dune files reflects \
               whether this option was passed or not.")
    and+ config_from_config_file = config_term
    and+ default_target =
      Arg.(
        value
        & opt dep
            (Dep.alias ~dir:Stdune.Path.Local.root
               Dune_engine.Alias.Name.default)
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
      Arg.(
        last
        & opt_all ~vopt:true bool [ false ]
        & info [ "promote-install-files" ] ~docs ~doc)
    and+ require_dune_project_file =
      let doc = "Fail if a dune-project file is missing." in
      Arg.(
        last
        & opt_all ~vopt:true bool [ false ]
        & info [ "require-dune-project-file" ] ~docs ~doc)
    in
    { root
    ; only_packages = No_restriction
    ; ignore_promoted_rules
    ; config_from_config_file
    ; profile = None
    ; default_target
    ; always_show_command_line
    ; promote_install_files
    ; require_dune_project_file
    }

  let dash_dash_release =
    Arg.(
      value
      & alias
          [ "--root"
          ; "."
          ; "--ignore-promoted-rules"
          ; "--no-config"
          ; "--profile"
          ; "release"
          ; "--always-show-command-line"
          ; "--promote-install-files"
          ; "--require-dune-project-file"
          ; "--default-target"
          ; "@install"
          ]
      & info [ "release" ] ~docs ~docv:"PACKAGES"
          ~doc:
            "Put $(b,dune) into a reproducible $(i,release) mode. This is in \
             fact a shorthand for $(b,--root . --ignore-promoted-rules \
             --no-config --profile release --always-show-command-line \
             --promote-install-files --default-target @install \
             --require-dune-project-file). You should use this option for \
             release builds. For instance, you must use this option in your \
             $(i,<package>.opam) files. Except if you already use $(b,-p), as \
             $(b,-p) implies this option.")

  let options =
    let+ t = options
    and+ _ = dash_dash_release
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
      match names with
      | None -> Only_packages.Clflags.No_restriction
      | Some names ->
        Restrict { names; command_line_option = "--only-packages" }
    in
    { t with only_packages }

  let dash_p =
    Term.with_used_args
      Arg.(
        value
        & alias_opt (fun s -> [ "--release"; "--only-packages"; s ])
        & info
            [ "p"; "for-release-of-packages" ]
            ~docs ~docv:"PACKAGES"
            ~doc:
              "Shorthand for $(b,--release --only-packages PACKAGE). You must \
               use this option in your $(i,<package>.opam) files, in order to \
               build only what's necessary when your project contains multiple \
               packages as well as getting reproducible builds.")

  let term =
    let+ t = options
    and+ _ = dash_p
    and+ profile =
      let doc =
        "Build profile. $(b,dev) if unspecified or $(b,release) if -p is set."
      in
      Arg.(
        last
        & opt_all (some profile) [ None ]
        & info [ "profile" ] ~docs
            ~env:(Arg.env_var ~doc "DUNE_PROFILE")
            ~doc:
              (Printf.sprintf
                 "Select the build profile, for instance $(b,dev) or \
                  $(b,release). The default is $(b,%s)."
                 (Profile.to_string Dune_rules.Profile.default)))
    in
    match profile with
    | None -> t
    | Some _ -> { t with profile }
end

let display_term =
  let module Display = Dune_engine.Scheduler.Config.Display in
  one_of
    (let+ verbose =
       Arg.(
         value & flag
         & info [ "verbose" ] ~docs:copts_sect
             ~doc:"Same as $(b,--display verbose)")
     in
     Option.some_if verbose { Display.verbosity = Verbose; status_line = true })
    Arg.(
      value
      & opt (some (enum Display.all)) None
      & info [ "display" ] ~docs:copts_sect ~docv:"MODE"
          ~doc:
            {|Control the display mode of Dune.
         See $(b,dune-config\(5\)) for more details.|})

let shared_with_config_file =
  let docs = copts_sect in
  let+ concurrency =
    let module Concurrency = Dune_config.Concurrency in
    let arg =
      Arg.conv
        ( (fun s ->
            Result.map_error (Concurrency.of_string s) ~f:(fun s -> `Msg s))
        , fun pp x -> Format.pp_print_string pp (Concurrency.to_string x) )
    in
    Arg.(
      value
      & opt (some arg) None
      & info [ "j" ] ~docs ~docv:"JOBS"
          ~doc:{|Run no more than $(i,JOBS) commands simultaneously.|})
  and+ sandboxing_preference =
    let all =
      List.map Dune_engine.Sandbox_mode.all_except_patch_back_source_tree
        ~f:(fun s -> (Dune_engine.Sandbox_mode.to_string s, s))
    in
    Arg.(
      value
      & opt (some (enum all)) None
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
                  (List.map
                     Dune_engine.Sandbox_mode.all_except_patch_back_source_tree
                     ~f:Dune_engine.Sandbox_mode.to_string))))
  and+ terminal_persistence =
    let modes = Dune_config.Terminal_persistence.all in
    let doc =
      let f s = fst s |> Printf.sprintf "$(b,%s)" in
      Printf.sprintf
        {|Changes how the log of build results are displayed to the
          console between rebuilds while in $(b,--watch) mode. Supported modes:
          %s.|}
        (List.map ~f modes |> String.concat ~sep:", ")
    in
    Arg.(
      value
      & opt (some (enum modes)) None
      & info [ "terminal-persistence" ] ~docs ~docv:"MODE" ~doc)
  and+ display = display_term
  and+ cache_enabled =
    let doc =
      Printf.sprintf "Enable or disable Dune cache (%s). Default is `%s'."
        (Arg.doc_alts_enum Dune_config.Cache.Enabled.all)
        (Dune_config.Cache.Enabled.to_string Dune_config.default.cache_enabled)
    in
    Arg.(
      value
      & opt (some (enum Dune_config.Cache.Enabled.all)) None
      & info [ "cache" ] ~docs ~env:(Arg.env_var ~doc "DUNE_CACHE") ~doc)
  and+ cache_storage_mode =
    let doc =
      Printf.sprintf "Dune cache storage mode (%s). Default is `%s'."
        (Arg.doc_alts_enum Dune_config.Cache.Storage_mode.all)
        (Dune_config.Cache.Storage_mode.to_string
           Dune_config.default.cache_storage_mode)
    in
    Arg.(
      value
      & opt (some (enum Dune_config.Cache.Storage_mode.all)) None
      & info [ "cache-storage-mode" ] ~docs
          ~env:(Arg.env_var ~doc "DUNE_CACHE_STORAGE_MODE")
          ~doc)
  and+ cache_check_probability =
    let doc =
      Printf.sprintf
        "Check build reproducibility by re-executing randomly chosen rules and \
         comparing their results with those stored in Dune cache. Note: by \
         increasing the probability of such checks you slow down the build. \
         The default probability is zero, i.e. no rules are checked."
    in
    Arg.(
      value
      & opt (some float) None
      & info
          [ "cache-check-probability" ]
          ~docs
          ~env:(Arg.env_var ~doc "DUNE_CACHE_CHECK_PROBABILITY")
          ~doc)
  and+ action_stdout_on_success =
    Arg.(
      value
      & opt (some (enum Dune_config.Action_output_on_success.all)) None
      & info
          [ "action-stdout-on-success" ]
          ~doc:
            "Specify how to deal with the standard output of actions when they \
             succeed. Possible values are: $(b,print) to just print it to \
             Dune's output, $(b,swallow) to completely ignore it and \
             $(b,must-be-empty) to enforce that the action printed nothing. \
             With $(b,must-be-empty), Dune will consider that the action \
             failed if it printed something to its standard output. The \
             default is $(b,print).")
  and+ action_stderr_on_success =
    Arg.(
      value
      & opt (some (enum Dune_config.Action_output_on_success.all)) None
      & info
          [ "action-stderr-on-success" ]
          ~doc:
            "Same as $(b,--action-stdout-on-success) but for the standard \
             output for error messages. A good default for large \
             mono-repositories is $(b,--action-stdout-on-success=swallow \
             --action-stderr-on-success=must-be-empty). This ensures that a \
             successful build has a \"clean\" empty output.")
  in
  { Dune_config.Partial.display
  ; concurrency
  ; sandboxing_preference = Option.map sandboxing_preference ~f:(fun x -> [ x ])
  ; terminal_persistence
  ; cache_enabled
  ; cache_reproducibility_check =
      Option.map cache_check_probability
        ~f:Dune_cache.Config.Reproducibility_check.check_with_probability
  ; cache_storage_mode
  ; action_stdout_on_success
  ; action_stderr_on_success
  }

module Cache_debug_flags = Dune_engine.Cache_debug_flags

let cache_debug_flags_term : Cache_debug_flags.t Term.t =
  let initial =
    { Cache_debug_flags.shared_cache = false
    ; workspace_local_cache = false
    ; fs_cache = false
    }
  in
  let all_layers =
    [ ("shared", fun r -> { r with Cache_debug_flags.shared_cache = true })
    ; ( "workspace-local"
      , fun r -> { r with Cache_debug_flags.workspace_local_cache = true } )
    ; ("fs", fun r -> { r with Cache_debug_flags.fs_cache = true })
    ]
  in
  let no_layers = ([], fun x -> x) in
  let combine_layers =
    List.fold_right ~init:no_layers
      ~f:(fun (names, value) (acc_names, acc_value) ->
        (names @ acc_names, fun x -> acc_value (value x)))
  in
  let all_layer_names = String.concat ~sep:"," (List.map ~f:fst all_layers) in
  let layers_conv =
    let parser s =
      let parse_one s =
        match
          List.find_map all_layers ~f:(fun (name, value) ->
              match String.equal name s with
              | true -> Some ([ name ], value)
              | false -> None)
        with
        | None ->
          ksprintf (fun s -> Error (`Msg s)) "Invalid cache layer name: %S" s
        | Some x -> Ok x
      in
      String.split s ~on:',' |> List.map ~f:parse_one |> Result.List.all
      |> Result.map ~f:combine_layers
    in
    let printer ppf (names, _value) =
      Format.pp_print_string ppf (String.concat ~sep:"," names)
    in
    Arg.conv ~docv:"CACHE-LAYERS" (parser, printer)
  in
  let+ _names, value =
    Arg.(
      value & opt layers_conv no_layers
      & info [ "debug-cache" ] ~docs:copts_sect
          ~doc:
            (sprintf
               {|Show debug messages on cache misses for the given cache layers.
Value is a comma-separated list of cache layer names.
All available cache layers: %s.|}
               all_layer_names))
  in
  value initial

let term ~default_root_is_cwd =
  let docs = copts_sect in
  let+ config_from_command_line = shared_with_config_file
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
  and+ debug_backtraces = debug_backtraces
  and+ debug_artifact_substitution =
    Arg.(
      value & flag
      & info
          [ "debug-artifact-substitution" ]
          ~docs ~doc:"Print debugging info about artifact substitution")
  and+ debug_digests =
    Arg.(
      value & flag
      & info [ "debug-digests" ] ~docs
          ~doc:"Explain why Dune decides to re-digest some files")
  and+ store_digest_preimage =
    Arg.(
      value & flag
      & info
          [ "debug-store-digest-preimage" ]
          ~docs
          ~doc:
            "Store digest preimage for all computed digests, so that it's \
             possible to reverse them later, for debugging. The digests are \
             stored in the shared cache (see --cache flag) as values, even if \
             cache is otherwise disabled. This should be used only for \
             debugging, since it's slow and it litters the shared cache.")
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
    let+ res =
      one_of
        (let+ watch =
           Arg.(
             value & flag
             & info [ "watch"; "w" ]
                 ~doc:
                   "Instead of terminating build after completion, wait \
                    continuously for file changes.")
         in
         if watch then Some Watch_mode_config.Eager else None)
        (let+ watch =
           Arg.(
             value & flag
             & info [ "passive-watch-mode" ]
                 ~doc:
                   "Similar to [--watch], but only start a build when \
                    instructed externally by an RPC.")
         in
         if watch then Some Watch_mode_config.Passive else None)
    in
    match res with
    | None -> Watch_mode_config.No
    | Some mode -> Watch_mode_config.Yes mode
  and+ print_metrics =
    Arg.(
      value & flag
      & info [ "print-metrics" ] ~docs
          ~doc:"Print out various performance metrics after every build")
  and+ dump_memo_graph_file =
    Arg.(
      value
      & opt (some string) None
      & info [ "dump-memo-graph" ] ~docs ~docv:"FILE"
          ~doc:
            "Dumps the dependency graph to a file after the build is complete")
  and+ dump_memo_graph_format =
    Arg.(
      value & opt graph_format Gexf
      & info
          [ "dump-memo-graph-format" ]
          ~docs ~docv:"FORMAT"
          ~doc:"File format to be used when dumping dependency graph")
  and+ dump_memo_graph_with_timing =
    Arg.(
      value & flag
      & info
          [ "dump-memo-graph-with-timing" ]
          ~docs
          ~doc:
            "With $(b,--dump-memo-graph), will re-run each cached node in the \
             Memo graph after building and include the runtime in the output. \
             Since all nodes contain a cached value, this will measure just \
             the runtime of each node")
  and+ { Options_implied_by_dash_p.root
       ; only_packages
       ; ignore_promoted_rules
       ; config_from_config_file
       ; profile
       ; default_target
       ; always_show_command_line
       ; promote_install_files
       ; require_dune_project_file
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
    let doc =
      "Shell command to use to diff files.\n\
      \                   Use - to disable printing the diff."
    in
    Arg.(
      value
      & opt (some string) None
      & info [ "diff-command" ] ~docs
          ~env:(Arg.env_var ~doc "DUNE_DIFF_COMMAND")
          ~doc)
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
  and+ () = build_info
  and+ instrument_with =
    let doc =
      {|"Enable instrumentation by $(b,BACKENDS).
        $(b,BACKENDS) is a comma-separated list of library names,
        each one of which must declare an instrumentation backend.|}
    in
    Arg.(
      value
      & opt (some (list lib_name)) None
      & info [ "instrument-with" ] ~docs
          ~env:(Arg.env_var ~doc "DUNE_INSTRUMENT_WITH")
          ~docv:"BACKENDS" ~doc)
  and+ file_watcher =
    let doc =
      {|Mechanism to detect changes in the source. Automatic to make dune run an
        external program to detect changes. Manual to notify dune that files
        have changed manually."|}
    in
    Arg.(
      value
      & opt
          (enum
             [ ("automatic", Dune_engine.Scheduler.Run.Automatic)
             ; ("manual", No_watcher)
             ])
          Automatic
      & info [ "file-watcher" ] ~doc)
  and+ wait_for_filesystem_clock =
    Arg.(
      value & flag
      & info
          [ "wait-for-filesystem-clock" ]
          ~doc:
            "Dune digest file contents for better incrementally. These digests \
             are themselves cached. In some cases, Dune needs to drop some \
             digest cache entries in order for things to be reliable. This \
             option makes Dune wait for the file system clock to advance so \
             that it doesn't need to drop anything. You should probably not \
             care about this option; it is mostly useful for Dune developers \
             to make Dune tests of the digest cache more reproducible.")
  and+ cache_debug_flags = cache_debug_flags_term
  and+ report_errors_config =
    Arg.(
      value
      & opt
          (enum
             [ ("early", Dune_engine.Report_errors_config.Early)
             ; ("deterministic", Deterministic)
             ; ("twice", Twice)
             ])
          Dune_engine.Report_errors_config.default
      & info [ "error-reporting" ]
          ~doc:
            "Controls when the build errors are reported.\n\
             $(b,early) - report errors as soon as they are discovered.\n\
             $(b,deterministic) - report errors at the end of the build in a \
             deterministic order.\n\
             $(b,twice) - report each error twice: once as soon as the error \
             is discovered and then again at the end of the build, in a \
             deterministic order.")
  and+ react_to_insignificant_changes =
    Arg.(
      value & flag
      & info
          [ "react-to-insignificant-changes" ]
          ~doc:
            "react to insignificant file system changes; this is only useful \
             for benchmarking dune")
  in
  let insignificant_changes =
    if react_to_insignificant_changes then `React else `Ignore
  in
  let build_dir = Option.value ~default:default_build_dir build_dir in
  let root =
    Workspace_root.create ~default_is_cwd:default_root_is_cwd
      ~specified_by_user:root
  in
  let stats =
    Option.map stats_trace_file ~f:(fun f ->
        let stats = Dune_stats.create (Out (open_out f)) in
        at_exit (fun () -> Dune_stats.close stats);
        stats)
  in
  let rpc = lazy (Dune_rpc_impl.Server.create ~root:root.dir stats) in
  if store_digest_preimage then Dune_engine.Reversible_digest.enable ();
  if print_metrics then (
    Memo.Perf_counters.enable ();
    Metrics.enable ());
  { debug_dep_path
  ; debug_findlib
  ; debug_backtraces
  ; debug_artifact_substitution
  ; debug_digests
  ; wait_for_filesystem_clock
  ; capture_outputs = not no_buffer
  ; root
  ; diff_command
  ; promote
  ; force
  ; ignore_promoted_rules
  ; only_packages
  ; rpc
  ; build_dir
  ; no_print_directory
  ; store_orig_src_dir
  ; default_target
  ; watch
  ; print_metrics
  ; dump_memo_graph_file
  ; dump_memo_graph_format
  ; dump_memo_graph_with_timing
  ; always_show_command_line
  ; promote_install_files
  ; stats
  ; file_watcher
  ; workspace_config =
      { x
      ; profile
      ; instrument_with
      ; workspace_file = Option.map workspace_file ~f:Arg.Path.path
      ; config_from_command_line
      ; config_from_config_file
      }
  ; cache_debug_flags
  ; report_errors_config
  ; require_dune_project_file
  ; insignificant_changes
  }

let term_with_default_root_is_cwd = term ~default_root_is_cwd:true

let term = term ~default_root_is_cwd:false

let config_from_config_file = Options_implied_by_dash_p.config_term

let context_arg ~doc =
  Arg.(
    value
    & opt Arg.context_name Dune_engine.Context_name.default
    & info [ "context" ] ~docv:"CONTEXT" ~doc)
