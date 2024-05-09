open Stdune
open Dune_config
open Dune_config_file
module Console = Dune_console
module Graph = Dune_graph.Graph
module Profile = Dune_lang.Profile

open struct
  open Dune_util
  module Execution_env = Execution_env
  module Log = Log
  module Report_error = Report_error
end

open struct
  open Dune_rules
  module Package = Package
  module Colors = Colors
  module Only_packages = Only_packages
end

open struct
  open Cmdliner
  module Cmd = Cmd
  module Term = Term
  module Manpage = Manpage
end

module Let_syntax = struct
  let ( let+ ) t f = Term.(const f $ t)
  let ( and+ ) a b = Term.(const (fun x y -> x, y) $ a $ b)
end

open Let_syntax

let copts_sect = "COMMON OPTIONS"

let debug_backtraces =
  Arg.(
    value
    & flag
    & info
        [ "debug-backtraces" ]
        ~docs:copts_sect
        ~doc:"Always print exception backtraces.")
;;

let default_build_dir = "_build"

let one_of term1 term2 =
  Term.ret
  @@ let+ x, args1 = Term.with_used_args term1
     and+ y, args2 = Term.with_used_args term2 in
     match args1, args2 with
     | _, [] -> `Ok x
     | [], _ -> `Ok y
     | arg1 :: _, arg2 :: _ ->
       `Error (true, sprintf "Cannot use %s and %s simultaneously" arg1 arg2)
;;

let build_info =
  let+ build_info =
    Arg.(
      value & flag & info [ "build-info" ] ~docs:"OPTIONS" ~doc:"Show build information.")
  in
  if build_info
  then (
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
;;

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
    ; ignore_lock_dir : bool
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
             & info
                 [ "config-file" ]
                 ~docs
                 ~docv:"FILE"
                 ~doc:"Load this configuration file instead of the default one.")
         in
         Option.map fn ~f:(fun fn -> This (Arg.Path.path fn)))
        (let+ x =
           Arg.(
             value
             & flag
             & info [ "no-config" ] ~docs ~doc:"Do not load the configuration file")
         in
         Option.some_if x No_config)
    in
    match Option.value x ~default:Default with
    | No_config -> Dune_config.Partial.empty
    | This fname -> Dune_config.load_config_file fname
    | Default ->
      if Execution_env.inside_dune
      then Dune_config.Partial.empty
      else Dune_config.load_user_config_file ()
  ;;

  let packages =
    let parser s =
      let parse_one s =
        match Package.Name.of_string_opt s with
        | Some x -> Ok x
        | None -> ksprintf (fun s -> Error (`Msg s)) "Invalid package name: %S" s
      in
      String.split s ~on:','
      |> List.map ~f:parse_one
      |> Result.List.all
      |> Result.map ~f:Package.Name.Set.of_list
    in
    let printer ppf set =
      Format.pp_print_string
        ppf
        (String.concat
           ~sep:","
           (Package.Name.Set.to_list set |> List.map ~f:Package.Name.to_string))
    in
    Arg.conv ~docv:"PACKAGES" (parser, printer)
  ;;

  let options =
    let+ root =
      Arg.(
        value
        & opt (some dir) None
        & info
            [ "root" ]
            ~docs
            ~docv:"DIR"
            ~doc:
              "Use this directory as workspace root instead of guessing it. Note that \
               this option doesn't change the interpretation of targets given on the \
               command line. It is only intended for scripts.")
    and+ ignore_promoted_rules =
      Arg.(
        value
        & flag
        & info
            [ "ignore-promoted-rules" ]
            ~docs
            ~doc:
              "Ignore rules with (mode promote), except ones with (only ...). The \
               variable %{ignoring_promoted_rules} in dune files reflects whether this \
               option was passed or not.")
    and+ config_from_config_file = config_term
    and+ default_target =
      Arg.(
        value
        & opt dep (Dep.alias ~dir:Stdune.Path.Local.root Dune_engine.Alias.Name.default)
        & info
            [ "default-target" ]
            ~docs
            ~docv:"TARGET"
            ~doc:
              "Set the default target that is used when none is specified to $(b,dune \
               build).")
    and+ always_show_command_line =
      let doc = "Always show the full command lines of programs executed by dune." in
      Arg.(value & flag & info [ "always-show-command-line" ] ~docs ~doc)
    and+ promote_install_files =
      let doc = "Promote any generated <package>.install files to the source tree." in
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
    and+ ignore_lock_dir =
      let doc = "Ignore dune.lock/ directory." in
      Arg.(value & flag & info [ "ignore-lock-dir" ] ~docs ~doc)
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
    ; ignore_lock_dir
    }
  ;;

  let dash_dash_release =
    let shorthand_for =
      [ "--root"
      ; "."
      ; "--ignore-promoted-rules"
      ; "--no-config"
      ; "--profile"
      ; "release"
      ; "--always-show-command-line"
      ; "--promote-install-files"
      ; "--require-dune-project-file"
      ; "--ignore-lock-dir"
      ; "--default-target"
      ; "@install"
      ]
    in
    Arg.(
      value
      & alias shorthand_for
      & info
          [ "release" ]
          ~docs
          ~docv:"PACKAGES"
          ~doc:
            (sprintf
               "Put $(b,dune) into a reproducible $(i,release) mode. Shorthand for \
                $(b,%s). You should use this option for release builds. For instance, \
                you must use this option in your $(i,<package>.opam) files. Except if \
                you already use $(b,-p), as $(b,-p) implies this option."
               (String.concat ~sep:" " shorthand_for)))
  ;;

  let options =
    let+ t = options
    and+ _ = dash_dash_release
    and+ only_packages =
      let+ names =
        Arg.(
          value
          & opt (some packages) None
          & info
              [ "only-packages" ]
              ~docs
              ~docv:"PACKAGES"
              ~doc:
                "Ignore stanzas referring to a package that is not in $(b,PACKAGES). \
                 $(b,PACKAGES) is a comma-separated list of package names. Note that \
                 this has the same effect as deleting the relevant stanzas from dune \
                 files. It is mostly meant for releases. During development, it is \
                 likely that what you want instead is to build a particular \
                 $(b,<package>.install) target.")
      in
      match names with
      | None -> Only_packages.Clflags.No_restriction
      | Some names -> Restrict { names; command_line_option = "--only-packages" }
    in
    { t with only_packages }
  ;;

  let dash_p =
    Term.with_used_args
      Arg.(
        value
        & alias_opt (fun s -> [ "--release"; "--only-packages"; s ])
        & info
            [ "p"; "for-release-of-packages" ]
            ~docs
            ~docv:"PACKAGES"
            ~doc:
              "Shorthand for $(b,--release --only-packages PACKAGE). You must use this \
               option in your $(i,<package>.opam) files, in order to build only what's \
               necessary when your project contains multiple packages as well as getting \
               reproducible builds.")
  ;;

  let term =
    let+ t = options
    and+ _ = dash_p
    and+ profile =
      let doc = "Build profile. $(b,dev) if unspecified or $(b,release) if -p is set." in
      Arg.(
        last
        & opt_all (some profile) [ None ]
        & info
            [ "profile" ]
            ~docs
            ~env:(Cmd.Env.info ~doc "DUNE_PROFILE")
            ~doc:
              (Printf.sprintf
                 "Select the build profile, for instance $(b,dev) or $(b,release). The \
                  default is $(b,%s)."
                 (Profile.to_string Profile.default)))
    in
    match profile with
    | None -> t
    | Some _ -> { t with profile }
  ;;
end

let display_term =
  let module Display = Dune_config.Display in
  one_of
    (let+ verbose =
       Arg.(
         value
         & flag
         & info [ "verbose" ] ~docs:copts_sect ~doc:"Same as $(b,--display verbose)")
     in
     Option.some_if verbose Dune_config.Display.verbose)
    Arg.(
      let doc =
        let all = Display.all |> List.map ~f:fst |> String.enumerate_or in
        sprintf
          "Control the display mode of Dune. See $(b,dune-config\\(5\\)) for more \
           details. Valid values for this option are %s."
          all
      in
      value
      & opt (some (enum Display.all)) None
      & info [ "display" ] ~docs:copts_sect ~docv:"MODE" ~doc)
;;

let shared_with_config_file =
  let docs = copts_sect in
  let+ concurrency =
    let module Concurrency = Dune_config.Concurrency in
    let arg =
      Arg.conv
        ( (fun s -> Result.map_error (Concurrency.of_string s) ~f:(fun s -> `Msg s))
        , fun pp x -> Format.pp_print_string pp (Concurrency.to_string x) )
    in
    Arg.(
      value
      & opt (some arg) None
      & info
          [ "j" ]
          ~docs
          ~docv:"JOBS"
          ~doc:"Run no more than $(i,JOBS) commands simultaneously.")
  and+ sandboxing_preference =
    let all =
      List.map Dune_engine.Sandbox_mode.all_except_patch_back_source_tree ~f:(fun s ->
        Dune_engine.Sandbox_mode.to_string s, s)
    in
    Arg.(
      value
      & opt (some (enum all)) None
      & info
          [ "sandbox" ]
          ~env:
            (Cmd.Env.info
               ~doc:"Sandboxing mode to use by default. (see --sandbox)"
               "DUNE_SANDBOX")
          ~doc:
            (Printf.sprintf
               "Set sandboxing mode. Some actions require a certain sandboxing mode, so \
                they will ignore this setting. The allowed values are: %s."
               (String.concat
                  ~sep:", "
                  (List.map
                     Dune_engine.Sandbox_mode.all_except_patch_back_source_tree
                     ~f:Dune_engine.Sandbox_mode.to_string))))
  and+ terminal_persistence =
    let modes = Dune_config.Terminal_persistence.all in
    let doc =
      let f s = fst s |> Printf.sprintf "$(b,%s)" in
      Printf.sprintf
        "Change how the log of build results are displayed to the console between \
         rebuilds while in $(b,--watch) mode. Supported modes: %s."
        (List.map ~f modes |> String.concat ~sep:", ")
    in
    Arg.(
      value
      & opt (some (enum modes)) None
      & info [ "terminal-persistence" ] ~docs ~docv:"MODE" ~doc)
  and+ display = display_term
  and+ cache_enabled =
    let doc =
      Printf.sprintf
        "Enable or disable Dune cache (%s). Default is `%s'."
        (Arg.doc_alts_enum Config.Toggle.all)
        (Config.Toggle.to_string Dune_config.default.cache_enabled)
    in
    Arg.(
      value
      & opt (some (enum Config.Toggle.all)) None
      & info [ "cache" ] ~docs ~env:(Cmd.Env.info ~doc "DUNE_CACHE") ~doc)
  and+ cache_storage_mode =
    let doc =
      Printf.sprintf
        "Dune cache storage mode (%s). Default is `%s'."
        (Arg.doc_alts_enum Dune_config.Cache.Storage_mode.all)
        (Dune_config.Cache.Storage_mode.to_string Dune_config.default.cache_storage_mode)
    in
    Arg.(
      value
      & opt (some (enum Dune_config.Cache.Storage_mode.all)) None
      & info
          [ "cache-storage-mode" ]
          ~docs
          ~env:(Cmd.Env.info ~doc "DUNE_CACHE_STORAGE_MODE")
          ~doc)
  and+ cache_check_probability =
    let doc =
      Printf.sprintf
        "Check build reproducibility by re-executing randomly chosen rules and comparing \
         their results with those stored in Dune cache. Note: by increasing the \
         probability of such checks you slow down the build. The default probability is \
         zero, i.e. no rules are checked."
    in
    Arg.(
      value
      & opt (some float) None
      & info
          [ "cache-check-probability" ]
          ~docs
          ~env:(Cmd.Env.info ~doc "DUNE_CACHE_CHECK_PROBABILITY")
          ~doc)
  and+ action_stdout_on_success =
    Arg.(
      value
      & opt (some (enum Dune_config.Action_output_on_success.all)) None
      & info
          [ "action-stdout-on-success" ]
          ~doc:
            "Specify how to deal with the standard output of actions when they succeed. \
             Possible values are: $(b,print) to just print it to Dune's output, \
             $(b,swallow) to completely ignore it and $(b,must-be-empty) to enforce that \
             the action printed nothing. With $(b,must-be-empty), Dune will consider \
             that the action failed if it printed something to its standard output. The \
             default is $(b,print).")
  and+ action_stderr_on_success =
    Arg.(
      value
      & opt (some (enum Dune_config.Action_output_on_success.all)) None
      & info
          [ "action-stderr-on-success" ]
          ~doc:
            "Same as $(b,--action-stdout-on-success) but for standard error instead of \
             standard output. A good default for large mono-repositories is \
             $(b,--action-stdout-on-success=swallow \
             --action-stderr-on-success=must-be-empty). This ensures that a successful \
             build has a \"clean\" empty output.")
  in
  { Dune_config.Partial.display
  ; concurrency
  ; sandboxing_preference = Option.map sandboxing_preference ~f:(fun x -> [ x ])
  ; terminal_persistence
  ; cache_enabled
  ; cache_reproducibility_check =
      Option.map
        cache_check_probability
        ~f:Dune_cache.Config.Reproducibility_check.check_with_probability
  ; cache_storage_mode
  ; action_stdout_on_success
  ; action_stderr_on_success
  ; experimental = None
  }
;;

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
  let no_layers = [], Fun.id in
  let combine_layers =
    List.fold_right ~init:no_layers ~f:(fun (names, value) (acc_names, acc_value) ->
      names @ acc_names, fun x -> acc_value (value x))
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
        | None -> ksprintf (fun s -> Error (`Msg s)) "Invalid cache layer name: %S" s
        | Some x -> Ok x
      in
      String.split s ~on:','
      |> List.map ~f:parse_one
      |> Result.List.all
      |> Result.map ~f:combine_layers
    in
    let printer ppf (names, _value) =
      Format.pp_print_string ppf (String.concat ~sep:"," names)
    in
    Arg.conv ~docv:"CACHE-LAYERS" (parser, printer)
  in
  let+ _names, value =
    Arg.(
      value
      & opt layers_conv no_layers
      & info
          [ "debug-cache" ]
          ~docs:copts_sect
          ~doc:
            (sprintf
               "Show debug messages on cache misses for the given cache layers. Value is \
                a comma-separated list of cache layer names. All available cache layers: \
                %s."
               all_layer_names))
  in
  value initial
;;

module Action_runner = struct
  type t =
    | No
    | Yes of
        (Dune_lang.Dep_conf.t Dune_rpc_impl.Server.t
         -> (Dune_engine.Action_exec.input -> Dune_engine.Action_runner.t option) Staged.t)
end

module Builder = struct
  type t =
    { debug_dep_path : bool
    ; debug_backtraces : bool
    ; debug_artifact_substitution : bool
    ; debug_load_dir : bool
    ; debug_digests : bool
    ; wait_for_filesystem_clock : bool
    ; only_packages : Only_packages.Clflags.t
    ; capture_outputs : bool
    ; diff_command : string option
    ; promote : Dune_engine.Clflags.Promote.t option
    ; ignore_promoted_rules : bool
    ; force : bool
    ; no_print_directory : bool
    ; ignore_lock_dir : bool
    ; store_orig_src_dir : bool
    ; default_target : Arg.Dep.t (* For build & runtest only *)
    ; watch : Dune_rpc_impl.Watch_mode_config.t
    ; print_metrics : bool
    ; dump_memo_graph_file : Path.External.t option
    ; dump_memo_graph_format : Graph.File_format.t
    ; dump_memo_graph_with_timing : bool
    ; dump_gc_stats : Path.External.t option
    ; always_show_command_line : bool
    ; promote_install_files : bool
    ; file_watcher : Dune_engine.Scheduler.Run.file_watcher
    ; workspace_config : Dune_rules.Workspace.Clflags.t
    ; cache_debug_flags : Dune_engine.Cache_debug_flags.t
    ; report_errors_config : Dune_engine.Report_errors_config.t
    ; separate_error_messages : bool
    ; stop_on_first_error : bool
    ; require_dune_project_file : bool
    ; watch_exclusions : string list
    ; build_dir : string
    ; root : string option
    ; stats_trace_file : string option
    ; stats_trace_extended : bool
    ; allow_builds : bool
    ; default_root_is_cwd : bool
    ; action_runner : Action_runner.t
    ; log_file : Dune_util.Log.File.t
    }

  let set_root t root = { t with root = Some root }
  let forbid_builds t = { t with allow_builds = false; no_print_directory = true }
  let set_default_root_is_cwd t x = { t with default_root_is_cwd = x }
  let set_action_runner t x = { t with action_runner = x }
  let set_log_file t x = { t with log_file = x }
  let disable_log_file t = { t with log_file = No_log_file }
  let set_promote t v = { t with promote = Some v }
  let default_target t = t.default_target

  (** Cmdliner documentation markup language
      (https://erratique.ch/software/cmdliner/doc/tool_man.html#doclang)
      requires that dollar signs (ex. $(tname)) and backslashes are escaped. *)
  let docmarkup_escape s =
    let b = Buffer.create (2 * String.length s) in
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | ('$' | '\\') as c ->
        Buffer.add_char b '\\';
        Buffer.add_char b c
      | c -> Buffer.add_char b c
    done;
    Buffer.contents b
  ;;

  let term =
    let docs = copts_sect in
    let+ config_from_command_line = shared_with_config_file
    and+ debug_dep_path =
      Arg.(
        value
        & flag
        & info
            [ "debug-dependency-path" ]
            ~docs
            ~doc:
              "In case of error, print the dependency path from the targets on the \
               command line to the rule that failed.")
    and+ debug_backtraces = debug_backtraces
    and+ debug_artifact_substitution =
      Arg.(
        value
        & flag
        & info
            [ "debug-artifact-substitution" ]
            ~docs
            ~doc:"Print debugging info about artifact substitution")
    and+ debug_load_dir =
      Arg.(
        value
        & flag
        & info
            [ "debug-load-dir" ]
            ~docs
            ~doc:"Print debugging info about directory loading")
    and+ debug_digests =
      Arg.(
        value
        & flag
        & info
            [ "debug-digests" ]
            ~docs
            ~doc:"Explain why Dune decides to re-digest some files")
    and+ no_buffer =
      let doc =
        "Do not buffer the output of commands executed by dune. By default dune buffers \
         the output of subcommands, in order to prevent interleaving when multiple \
         commands are executed in parallel. However, this can be an issue when debugging \
         long running tests. With $(b,--no-buffer), commands have direct access to the \
         terminal. Note that as a result their output won't be captured in the log file. \
         You should use this option in conjunction with $(b,-j 1), to avoid \
         interleaving. Additionally you should use $(b,--verbose) as well, to make sure \
         that commands are printed before they are being executed."
      in
      Arg.(value & flag & info [ "no-buffer" ] ~docs ~docv:"DIR" ~doc)
    and+ workspace_file =
      let doc = "Use this specific workspace file instead of looking it up." in
      Arg.(
        value
        & opt (some external_path) None
        & info
            [ "workspace" ]
            ~docs
            ~docv:"FILE"
            ~doc
            ~env:(Cmd.Env.info ~doc "DUNE_WORKSPACE"))
    and+ promote =
      one_of
        (let+ auto =
           Arg.(
             value
             & flag
             & info
                 [ "auto-promote" ]
                 ~docs
                 ~doc:
                   "Automatically promote files. This is similar to running $(b,dune \
                    promote) after the build.")
         in
         Option.some_if auto Dune_engine.Clflags.Promote.Automatically)
        (let+ disable =
           let doc = "Disable all promotion rules" in
           let env = Cmd.Env.info ~doc "DUNE_DISABLE_PROMOTION" in
           Arg.(value & flag & info [ "disable-promotion" ] ~docs ~env ~doc)
         in
         Option.some_if disable Dune_engine.Clflags.Promote.Never)
    and+ force =
      Arg.(
        value
        & flag
        & info
            [ "force"; "f" ]
            ~doc:
              "Force actions associated to aliases to be re-executed even if their \
               dependencies haven't changed.")
    and+ watch =
      let+ res =
        one_of
          (let+ watch =
             Arg.(
               value
               & flag
               & info
                   [ "watch"; "w" ]
                   ~doc:
                     "Instead of terminating build after completion, wait continuously \
                      for file changes.")
           in
           if watch then Some Dune_rpc_impl.Watch_mode_config.Eager else None)
          (let+ watch =
             Arg.(
               value
               & flag
               & info
                   [ "passive-watch-mode" ]
                   ~doc:
                     "Similar to [--watch], but only start a build when instructed \
                      externally by an RPC.")
           in
           if watch then Some Dune_rpc_impl.Watch_mode_config.Passive else None)
      in
      match res with
      | None -> Dune_rpc_impl.Watch_mode_config.No
      | Some mode -> Yes mode
    and+ print_metrics =
      Arg.(
        value
        & flag
        & info
            [ "print-metrics" ]
            ~docs
            ~doc:"Print out various performance metrics after every build.")
    and+ dump_memo_graph_file =
      Arg.(
        value
        & opt (some string) None
        & info
            [ "dump-memo-graph" ]
            ~docs
            ~docv:"FILE"
            ~doc:"Dump the dependency graph to a file after the build is complete.")
    and+ dump_memo_graph_format =
      Arg.(
        value
        & opt graph_format Gexf
        & info
            [ "dump-memo-graph-format" ]
            ~docs
            ~docv:"FORMAT"
            ~doc:"Set the file format used by $(b,--dump-memo-graph)")
    and+ dump_memo_graph_with_timing =
      Arg.(
        value
        & flag
        & info
            [ "dump-memo-graph-with-timing" ]
            ~docs
            ~doc:
              "Re-run each cached node in the Memo graph after building and include the \
               run duration in the output of $(b,--dump-memo-graph). Since all nodes \
               contain a cached value, each measurement will only account for a single \
               node.")
    and+ dump_gc_stats =
      Arg.(
        value
        & opt (some string) None
        & info
            [ "dump-gc-stats" ]
            ~docs
            ~docv:"FILE"
            ~doc:"Dump the garbage collector stats to a file after the build is complete.")
    and+ { Options_implied_by_dash_p.root
         ; only_packages
         ; ignore_promoted_rules
         ; config_from_config_file
         ; profile
         ; default_target
         ; always_show_command_line
         ; promote_install_files
         ; require_dune_project_file
         ; ignore_lock_dir
         }
      =
      Options_implied_by_dash_p.term
    and+ x =
      Arg.(
        value
        & opt (some Arg.context_name) None
        & info [ "x" ] ~docs ~doc:"Cross-compile using this toolchain.")
    and+ build_dir =
      let doc = "Specified build directory. _build if unspecified" in
      Arg.(
        value
        & opt (some string) None
        & info
            [ "build-dir" ]
            ~docs
            ~docv:"FILE"
            ~env:(Cmd.Env.info ~doc "DUNE_BUILD_DIR")
            ~doc)
    and+ diff_command =
      let doc =
        "Shell command to use to diff files. Use - to disable printing the diff."
      in
      Arg.(
        value
        & opt (some string) None
        & info [ "diff-command" ] ~docs ~env:(Cmd.Env.info ~doc "DUNE_DIFF_COMMAND") ~doc)
    and+ stats_trace_file =
      Arg.(
        value
        & opt (some string) None
        & info
            [ "trace-file" ]
            ~docs
            ~docv:"FILE"
            ~doc:
              "Output trace data in catapult format (compatible with chrome://tracing).")
    and+ stats_trace_extended =
      Arg.(
        value
        & flag
        & info
            [ "trace-extended" ]
            ~docs
            ~doc:"Output extended trace data (requires trace-file).")
    and+ no_print_directory =
      Arg.(
        value
        & flag
        & info
            [ "no-print-directory" ]
            ~docs
            ~doc:"Suppress \"Entering directory\" messages.")
    and+ store_orig_src_dir =
      let doc = "Store original source location in dune-package metadata." in
      Arg.(
        value
        & flag
        & info
            [ "store-orig-source-dir" ]
            ~docs
            ~env:(Cmd.Env.info ~doc "DUNE_STORE_ORIG_SOURCE_DIR")
            ~doc)
    and+ () = build_info
    and+ instrument_with =
      let doc =
        "Enable instrumentation by $(b,BACKENDS). $(b,BACKENDS) is a comma-separated \
         list of library names, each one of which must declare an instrumentation \
         backend."
      in
      Arg.(
        value
        & opt (some (list lib_name)) None
        & info
            [ "instrument-with" ]
            ~docs
            ~env:(Cmd.Env.info ~doc "DUNE_INSTRUMENT_WITH")
            ~docv:"BACKENDS"
            ~doc)
    and+ file_watcher =
      let doc =
        "Mechanism to detect changes in the source. Automatic to make dune run an \
         external program to detect changes. Manual to notify dune that files have \
         changed manually."
      in
      Arg.(
        value
        & opt
            (enum
               [ "automatic", Dune_engine.Scheduler.Run.Automatic; "manual", No_watcher ])
            Automatic
        & info [ "file-watcher" ] ~doc)
    and+ watch_exclusions =
      let std_exclusions = Dune_config.standard_watch_exclusions in
      let doc =
        let escaped_std_exclusions = List.map ~f:docmarkup_escape std_exclusions in
        "Adds a POSIX regular expression that will exclude matching directories from \
         $(b,`dune build --watch`). The option $(opt) can be repeated to add multiple \
         exclusions. Semicolons can be also used as a separator. If no exclusions are \
         provided, then a standard set of exclusions is used; however, if $(i,one or \
         more) $(opt) are used, $(b,none) of the standard exclusions are used. The \
         standard exclusions are: "
        ^ String.concat ~sep:" " escaped_std_exclusions
      in
      let arg =
        Arg.(
          value
          & opt_all (list ~sep:';' string) [ std_exclusions ]
          & info [ "watch-exclusions" ] ~docs ~docv:"REGEX" ~doc)
      in
      Term.(const List.flatten $ arg)
    and+ wait_for_filesystem_clock =
      Arg.(
        value
        & flag
        & info
            [ "wait-for-filesystem-clock" ]
            ~doc:
              "Dune digest file contents for better incrementally. These digests are \
               themselves cached. In some cases, Dune needs to drop some digest cache \
               entries in order for things to be reliable. This option makes Dune wait \
               for the file system clock to advance so that it doesn't need to drop \
               anything. You should probably not care about this option; it is mostly \
               useful for Dune developers to make Dune tests of the digest cache more \
               reproducible.")
    and+ cache_debug_flags = cache_debug_flags_term
    and+ report_errors_config =
      Arg.(
        value
        & opt
            (enum
               [ "early", Dune_engine.Report_errors_config.Early
               ; "deterministic", Deterministic
               ; "twice", Twice
               ])
            Dune_engine.Report_errors_config.default
        & info
            [ "error-reporting" ]
            ~doc:
              "Controls when the build errors are reported. $(b,early) reports errors as \
               soon as they are discovered. $(b,deterministic) reports errors at the end \
               of the build in a deterministic order. $(b,twice) reports each error \
               twice: once as soon as the error is discovered and then again at the end \
               of the build, in a deterministic order.")
    and+ separate_error_messages =
      Arg.(
        value
        & flag
        & info
            [ "display-separate-messages" ]
            ~doc:"Separate error messages with a blank line.")
    and+ stop_on_first_error =
      Arg.(
        value
        & flag
        & info
            [ "stop-on-first-error" ]
            ~doc:"Stop the build as soon as an error is encountered.")
    in
    if Option.is_none stats_trace_file && stats_trace_extended
    then User_error.raise [ Pp.text "--trace-extended can only be used with --trace" ];
    { debug_dep_path
    ; debug_backtraces
    ; debug_artifact_substitution
    ; debug_load_dir
    ; debug_digests
    ; wait_for_filesystem_clock
    ; only_packages
    ; capture_outputs = not no_buffer
    ; diff_command
    ; promote
    ; ignore_promoted_rules
    ; force
    ; no_print_directory
    ; ignore_lock_dir
    ; store_orig_src_dir
    ; default_target
    ; watch
    ; print_metrics
    ; dump_memo_graph_file =
        Option.map
          dump_memo_graph_file
          ~f:Path.External.of_filename_relative_to_initial_cwd
    ; dump_memo_graph_format
    ; dump_memo_graph_with_timing
    ; dump_gc_stats =
        Option.map dump_gc_stats ~f:Path.External.of_filename_relative_to_initial_cwd
    ; always_show_command_line
    ; promote_install_files
    ; file_watcher
    ; workspace_config =
        { x
        ; profile
        ; instrument_with
        ; workspace_file =
            Option.map workspace_file ~f:(fun p ->
              Path.Outside_build_dir.External (Arg.Path.External.path p))
        ; config_from_command_line
        ; config_from_config_file
        }
    ; cache_debug_flags
    ; report_errors_config
    ; separate_error_messages
    ; stop_on_first_error
    ; require_dune_project_file
    ; watch_exclusions
    ; build_dir = Option.value ~default:default_build_dir build_dir
    ; root
    ; stats_trace_file
    ; stats_trace_extended
    ; allow_builds = true
    ; default_root_is_cwd = false
    ; action_runner = No
    ; log_file = Default
    }
  ;;
end

type t =
  { builder : Builder.t
  ; root : Workspace_root.t
  ; rpc :
      [ `Allow of Dune_lang.Dep_conf.t Dune_rpc_impl.Server.t Lazy.t | `Forbid_builds ]
  ; stats : Dune_stats.t option
  }

let capture_outputs t = t.builder.capture_outputs
let root t = t.root
let watch t = t.builder.watch
let x t = t.builder.workspace_config.x
let print_metrics t = t.builder.print_metrics
let dump_memo_graph_file t = t.builder.dump_memo_graph_file
let dump_memo_graph_format t = t.builder.dump_memo_graph_format
let dump_memo_graph_with_timing t = t.builder.dump_memo_graph_with_timing
let file_watcher t = t.builder.file_watcher
let prefix_target t s = t.root.reach_from_root_prefix ^ s

let rpc t =
  match t.rpc with
  | `Forbid_builds -> `Forbid_builds
  | `Allow rpc -> `Allow (Lazy.force rpc)
;;

let watch_exclusions t = t.builder.watch_exclusions
let stats t = t.stats

(* To avoid needless recompilations under Windows, where the case of
   [Sys.getcwd] can vary between different invocations of [dune], normalize to
   lowercase. *)
let normalize_path path =
  if Sys.win32
  then (
    let src = Path.External.to_string path in
    let is_letter = function
      | 'a' .. 'z' | 'A' .. 'Z' -> true
      | _ -> false
    in
    if String.length src >= 2 && is_letter src.[0] && src.[1] = ':'
    then (
      let dst = Bytes.create (String.length src) in
      Bytes.set dst 0 (Char.uppercase_ascii src.[0]);
      Bytes.blit_string ~src ~src_pos:1 ~dst ~dst_pos:1 ~len:(String.length src - 1);
      Path.External.of_string (Bytes.unsafe_to_string dst))
    else path)
  else path
;;

let print_entering_message c =
  let cwd = Path.to_absolute_filename Path.root in
  if cwd <> Fpath.initial_cwd && not c.builder.no_print_directory
  then (
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
      match Execution_env.inside_dune with
      | false -> cwd
      | true ->
        let descendant_simple p ~of_ =
          match String.drop_prefix p ~prefix:of_ with
          | None | Some "" -> None
          | Some s -> Some (String.drop s 1)
        in
        (match descendant_simple cwd ~of_:Fpath.initial_cwd with
         | Some s -> s
         | None ->
           (match descendant_simple Fpath.initial_cwd ~of_:cwd with
            | None -> cwd
            | Some s ->
              let rec loop acc dir =
                if dir = Filename.current_dir_name
                then acc
                else loop (Filename.concat acc "..") (Filename.dirname dir)
              in
              loop ".." (Filename.dirname s)))
    in
    Console.print [ Pp.verbatim (sprintf "Entering directory '%s'" dir) ];
    at_exit (fun () ->
      flush stdout;
      Console.print [ Pp.verbatim (sprintf "Leaving directory '%s'" dir) ]))
;;

(* CR-someday rleshchinskiy: The split between `build` and `init` seems quite arbitrary,
   we should probably refactor that at some point. *)
let build (builder : Builder.t) =
  let root =
    Workspace_root.create
      ~default_is_cwd:builder.default_root_is_cwd
      ~specified_by_user:builder.root
  in
  let stats =
    Option.map builder.stats_trace_file ~f:(fun f ->
      let stats =
        Dune_stats.create
          ~extended_build_job_info:builder.stats_trace_extended
          (Out (open_out f))
      in
      Dune_stats.set_global stats;
      stats)
  in
  let rpc =
    if builder.allow_builds
    then
      `Allow
        (lazy
          (let registry =
             match builder.watch with
             | Yes _ -> `Add
             | No -> `Skip
           in
           let lock_timeout =
             match builder.watch with
             | Yes Passive -> Some 1.0
             | _ -> None
           in
           let action_runner = Dune_engine.Action_runner.Rpc_server.create () in
           Dune_rpc_impl.Server.create
             ~lock_timeout
             ~registry
             ~root:root.dir
             ~handle:Dune_rules_rpc.register
             ~watch_mode_config:builder.watch
             ~parse_build:Dune_rules_rpc.parse_build
             stats
             action_runner))
    else `Forbid_builds
  in
  if builder.print_metrics then Dune_metrics.enable ();
  { builder; root; rpc; stats }
;;

let init (builder : Builder.t) =
  let c = build builder in
  if c.root.dir <> Filename.current_dir_name then Sys.chdir c.root.dir;
  Path.set_root (normalize_path (Path.External.cwd ()));
  Path.Build.set_build_dir (Path.Outside_build_dir.of_string c.builder.build_dir);
  (* Once we have the build directory set, initialise the logging. We can't do
     this earlier, because the build log typically goes into [_build/log]. *)
  Log.init () ~file:builder.log_file;
  (* We need to print this before reading the workspace file, so that the editor
     can interpret errors in the workspace file. *)
  print_entering_message c;
  Dune_rules.Workspace.Clflags.set c.builder.workspace_config;
  let config =
    (* Here we make the assumption that this computation doesn't yield. *)
    Fiber.run
      (Memo.run (Dune_rules.Workspace.workspace_config ()))
      ~iter:(fun () -> assert false)
  in
  let config =
    Dune_config.adapt_display
      config
      ~output_is_a_tty:(Lazy.force Ansi_color.output_is_a_tty)
  in
  Dune_config.init
    config
    ~watch:
      (match c.builder.watch with
       | No -> false
       | Yes _ -> true);
  Dune_engine.Execution_parameters.init
    (let open Memo.O in
     let+ w = Dune_rules.Workspace.workspace () in
     Dune_engine.Execution_parameters.builtin_default
     |> Dune_rules.Workspace.update_execution_parameters w);
  Dune_rules.Global.init ~capture_outputs:c.builder.capture_outputs;
  let cache_config =
    match config.cache_enabled with
    | `Disabled -> Dune_cache.Config.Disabled
    | `Enabled ->
      Enabled
        { storage_mode = Option.value config.cache_storage_mode ~default:Hardlink
        ; reproducibility_check = config.cache_reproducibility_check
        }
  in
  Log.info [ Pp.textf "Shared cache: %s" (Config.Toggle.to_string config.cache_enabled) ];
  Log.info
    [ Pp.textf
        "Shared cache location: %s"
        (Path.to_string Dune_cache_storage.Layout.root_dir)
    ];
  let action_runner =
    match builder.action_runner with
    | No -> None
    | Yes f ->
      (match rpc c with
       | `Forbid_builds -> Code_error.raise "action runners require building" []
       | `Allow server -> Some (Staged.unstage @@ f server))
  in
  Dune_rules.Main.init
    ?action_runner
    ~stats:c.stats
    ~sandboxing_preference:config.sandboxing_preference
    ~cache_config
    ~cache_debug_flags:c.builder.cache_debug_flags
    ();
  Only_packages.Clflags.set c.builder.only_packages;
  Report_error.print_memo_stacks := c.builder.debug_dep_path;
  Dune_engine.Clflags.report_errors_config := c.builder.report_errors_config;
  Dune_engine.Clflags.debug_backtraces c.builder.debug_backtraces;
  Dune_rules.Clflags.debug_artifact_substitution := c.builder.debug_artifact_substitution;
  Dune_engine.Clflags.debug_load_dir := c.builder.debug_load_dir;
  Dune_engine.Clflags.debug_fs_cache := c.builder.cache_debug_flags.fs_cache;
  Dune_digest.Clflags.debug_digests := c.builder.debug_digests;
  Dune_digest.Clflags.wait_for_filesystem_clock := c.builder.wait_for_filesystem_clock;
  Dune_engine.Clflags.capture_outputs := c.builder.capture_outputs;
  Dune_engine.Clflags.diff_command := c.builder.diff_command;
  Dune_engine.Clflags.promote := c.builder.promote;
  Dune_engine.Clflags.force := c.builder.force;
  Dune_engine.Clflags.stop_on_first_error := c.builder.stop_on_first_error;
  Dune_rules.Clflags.store_orig_src_dir := c.builder.store_orig_src_dir;
  Dune_rules.Clflags.promote_install_files := c.builder.promote_install_files;
  Dune_engine.Clflags.always_show_command_line := c.builder.always_show_command_line;
  Dune_rules.Clflags.ignore_promoted_rules := c.builder.ignore_promoted_rules;
  Dune_rules.Clflags.ignore_lock_dir := c.builder.ignore_lock_dir;
  Dune_rules.Clflags.on_missing_dune_project_file
  := if c.builder.require_dune_project_file then Error else Warn;
  Log.info
    [ Pp.textf
        "Workspace root: %s"
        (Path.to_absolute_filename Path.root |> String.maybe_quoted)
    ];
  Dune_console.separate_messages c.builder.separate_error_messages;
  Option.iter c.stats ~f:(fun stats ->
    if Dune_stats.extended_build_job_info stats
    then
      (* Communicate config settings as an instant event here. *)
      let open Chrome_trace in
      let args = [ "build_dir", `String (Path.Build.to_string Path.Build.root) ] in
      let ts = Event.Timestamp.of_float_seconds (Unix.gettimeofday ()) in
      let common = Event.common_fields ~cat:[ "config" ] ~name:"config" ~ts () in
      let event = Event.instant ~args common in
      Dune_stats.emit stats event);
  (* Setup hook for printing GC stats to a file *)
  at_exit (fun () ->
    match c.builder.dump_gc_stats with
    | None -> ()
    | Some file ->
      Gc.full_major ();
      Gc.compact ();
      let stat = Gc.stat () in
      let path = Path.external_ file in
      Dune_util.Gc.serialize ~path stat);
  c, config
;;

let footer =
  `Blocks [ `S "BUGS"; `P "Check bug reports at https://github.com/ocaml/dune/issues" ]
;;

let examples = function
  | [] -> `Blocks []
  | _ :: _ as examples ->
    let block_of_example index (intro, ex) =
      let prose = `I (Int.to_string (index + 1) ^ ".", String.trim intro ^ ":")
      and code_lines =
        ex
        |> String.trim
        |> String.split_lines
        |> List.concat_map ~f:(fun codeline -> [ `Noblank; `Pre ("      " ^ codeline) ])
        (* suppress initial blank *)
        |> List.tl
      in
      `Blocks (prose :: code_lines)
    in
    let example_blocks = examples |> List.mapi ~f:block_of_example in
    `Blocks (`S Cmdliner.Manpage.s_examples :: example_blocks)
;;

(* Short reminders for the most used and useful commands *)
let command_synopsis commands =
  let format_command c acc = `Noblank :: `P (Printf.sprintf "$(b,dune %s)" c) :: acc in
  [ `S "SYNOPSIS"; `Blocks (List.fold_right ~init:[] ~f:format_command commands) ]
;;

let help_secs =
  [ `S copts_sect
  ; `P "These options are common to all commands."
  ; `S "MORE HELP"
  ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
  ; footer
  ]
;;

let envs =
  Cmd.Env.
    [ info
        ~doc:
          "If different than $(b,0), ANSI colors are supported and should be used when \
           the program isn't piped. If equal to $(b,0), don't output ANSI color escape \
           codes"
        "CLICOLOR"
    ; info
        ~doc:"If different than $(b,0), ANSI colors should be enabled no matter what."
        "CLICOLOR_FORCE"
    ; info
        "DUNE_CACHE_ROOT"
        ~doc:"If set, determines the location of the machine-global shared cache."
    ]
;;

let config_from_config_file = Options_implied_by_dash_p.config_term

let context_arg ~doc =
  Arg.(
    value
    & opt Arg.context_name Dune_engine.Context_name.default
    & info [ "context" ] ~docv:"CONTEXT" ~doc)
;;
