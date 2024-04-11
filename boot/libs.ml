let external_libraries = [ "unix"; "threads" ]

let local_libraries =
  [ ("otherlibs/ordering", Some "Ordering", false, None)
  ; ("vendor/pp/src", Some "Pp", false, None)
  ; ("otherlibs/dyn", Some "Dyn", false, None)
  ; ("otherlibs/stdune/dune_filesystem_stubs", Some "Dune_filesystem_stubs",
    false, None)
  ; ("vendor/csexp/src", Some "Csexp", false, None)
  ; ("otherlibs/stdune/src", Some "Stdune", false, None)
  ; ("src/dune_graph", Some "Dune_graph", false, None)
  ; ("vendor/incremental-cycles/src", Some "Incremental_cycles", false, None)
  ; ("src/dag", Some "Dag", false, None)
  ; ("vendor/fiber/src", Some "Fiber", false, None)
  ; ("src/dune_console", Some "Dune_console", false, None)
  ; ("src/memo", Some "Memo", false, None)
  ; ("vendor/uutf", None, false, None)
  ; ("src/dune_sexp", Some "Dune_sexp", false, None)
  ; ("src/ocaml-config", Some "Ocaml_config", false, None)
  ; ("src/ocaml", Some "Ocaml", false, None)
  ; ("vendor/re/src", Some "Dune_re", false, None)
  ; ("otherlibs/dune-glob/src", Some "Dune_glob", false, None)
  ; ("otherlibs/xdg", Some "Xdg", false, None)
  ; ("otherlibs/dune-rpc/private", Some "Dune_rpc_private", false, None)
  ; ("src/dune_config", Some "Dune_config", false, None)
  ; ("vendor/sha", None, false, None)
  ; ("vendor/opam/src/core", None, false, None)
  ; ("vendor/opam-file-format", None, false, None)
  ; ("vendor/opam/src/format", None, false, None)
  ; ("otherlibs/chrome-trace/src", Some "Chrome_trace", false, None)
  ; ("vendor/spawn/src", Some "Spawn", false, None)
  ; ("src/dune_stats", Some "Dune_stats", false, None)
  ; ("vendor/build_path_prefix_map/src", Some "Build_path_prefix_map", false,
    None)
  ; ("src/dune_util", Some "Dune_util", false, None)
  ; ("src/dune_metrics", Some "Dune_metrics", false, None)
  ; ("src/dune_digest", Some "Dune_digest", false, None)
  ; ("src/predicate_lang", Some "Predicate_lang", false, None)
  ; ("otherlibs/dune-private-libs/section", Some "Dune_section", false, None)
  ; ("src/dune_lang", Some "Dune_lang", false, None)
  ; ("src/fiber_event_bus", Some "Fiber_event_bus", false, None)
  ; ("src/dune_async_io", Some "Dune_async_io", false, None)
  ; ("src/fiber_util", Some "Fiber_util", false, None)
  ; ("src/dune_cache_storage", Some "Dune_cache_storage", false, None)
  ; ("src/dune_targets", Some "Dune_targets", false, None)
  ; ("src/dune_cache", Some "Dune_cache", false, None)
  ; ("otherlibs/dune-action-plugin/src", Some "Dune_action_plugin", false,
    None)
  ; ("src/dune_output_truncation", Some "Dune_output_truncation", false,
    None)
  ; ("src/csexp_rpc", Some "Csexp_rpc", false, None)
  ; ("src/dune_rpc_server", Some "Dune_rpc_server", false, None)
  ; ("src/dune_rpc_client", Some "Dune_rpc_client", false, None)
  ; ("src/dune_thread_pool", Some "Dune_thread_pool", false, None)
  ; ("otherlibs/ocamlc-loc/src", Some "Ocamlc_loc", false, None)
  ; ("src/fsevents", Some "Fsevents", false, None)
  ; ("vendor/ocaml-inotify/src", Some "Ocaml_inotify", false, None)
  ; ("src/async_inotify_for_dune", Some "Async_inotify_for_dune", false,
    None)
  ; ("src/fswatch_win", Some "Fswatch_win", false, None)
  ; ("src/dune_file_watcher", Some "Dune_file_watcher", false, None)
  ; ("src/dune_engine", Some "Dune_engine", false, None)
  ; ("otherlibs/dune-private-libs/meta_parser", Some "Dune_meta_parser",
    false, None)
  ; ("src/fs", Some "Fs", false, None)
  ; ("src/dune_findlib", Some "Dune_findlib", false, None)
  ; ("src/dune_vcs", Some "Dune_vcs", false, None)
  ; ("vendor/opam/src/repository", None, false, None)
  ; ("vendor/opam/src/state", None, false, None)
  ; ("vendor/0install-solver/src/solver", Some "Zeroinstall_solver", false,
    None)
  ; ("vendor/fmt/src", None, false, None)
  ; ("vendor/opam-0install/lib", Some "Opam_0install", false, None)
  ; ("otherlibs/dune-build-info/src", Some "Build_info", false,
    Some "Build_info_data")
  ; ("src/dune_pkg", Some "Dune_pkg", false, None)
  ; ("src/install", Some "Install", false, None)
  ; ("otherlibs/dune-site/src/private", Some "Dune_site_private", false,
    None)
  ; ("src/dune_threaded_console", Some "Dune_threaded_console", false, None)
  ; ("vendor/lwd/lwd", None, false, None)
  ; ("vendor/notty/src", None, true, None)
  ; ("vendor/notty/src-unix", None, true, None)
  ; ("vendor/lwd/nottui", None, false, None)
  ; ("src/dune_tui", Some "Dune_tui", true, None)
  ; ("src/dune_config_file", Some "Dune_config_file", false, None)
  ; ("src/dune_patch", Some "Dune_patch", false, None)
  ; ("src/scheme", Some "Scheme", false, None)
  ; ("src/dune_rules", Some "Dune_rules", true, None)
  ; ("src/upgrader", Some "Dune_upgrader", false, None)
  ; ("src/dune_pkg_outdated", Some "Dune_pkg_outdated", false, None)
  ; ("vendor/cmdliner/src", None, false, None)
  ; ("src/dune_rpc_impl", Some "Dune_rpc_impl", false, None)
  ; ("src/dune_rules_rpc", Some "Dune_rules_rpc", false, None)
  ]

let build_flags =
  [ ([ "win32"; "win64"; "mingw"; "mingw64" ],
    [ "-ccopt"; "-D_UNICODE"; "-ccopt"; "-DUNICODE" ])
  ]

let link_flags =
  [ ([ "macosx" ],
    [ "-cclib"
    ; "-framework CoreFoundation"
    ; "-cclib"
    ; "-framework CoreServices"
    ])
  ; ([ "win32"; "win64"; "mingw"; "mingw64" ],
    [ "-cclib"; "-lshell32"; "-cclib"; "-lole32"; "-cclib"; "-luuid" ])
  ; ([ "beos" ], [ "-cclib"; "-lbsd" ])
  ]
