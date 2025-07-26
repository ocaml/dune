
type library =
  { path : string
  ; main_module_name : string option
  ; include_subdirs_unqualified : bool
  ; special_builtin_support : string option
  }

let external_libraries = [ "unix"; "threads" ]

let local_libraries =
  [ { path = "otherlibs/ordering"
    ; main_module_name = Some "Ordering"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "vendor/pp/src"
    ; main_module_name = Some "Pp"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "otherlibs/dyn"
    ; main_module_name = Some "Dyn"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "vendor/csexp/src"
    ; main_module_name = Some "Csexp"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "otherlibs/stdune/src"
    ; main_module_name = Some "Stdune"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_graph"
    ; main_module_name = Some "Dune_graph"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "vendor/incremental-cycles/src"
    ; main_module_name = Some "Incremental_cycles"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dag"
    ; main_module_name = Some "Dag"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/fiber/src"
    ; main_module_name = Some "Fiber"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_console"
    ; main_module_name = Some "Dune_console"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/memo"
    ; main_module_name = Some "Memo"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_config"
    ; main_module_name = Some "Dune_config"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_async_io"
    ; main_module_name = Some "Dune_async_io"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "vendor/re/src"
    ; main_module_name = Some "Dune_re"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "otherlibs/dune-glob/src"
    ; main_module_name = Some "Dune_glob"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_metrics"
    ; main_module_name = Some "Dune_metrics"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "vendor/ocaml-blake3-mini"
    ; main_module_name = Some "Blake3_mini"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "otherlibs/chrome-trace/src"
    ; main_module_name = Some "Chrome_trace"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "vendor/spawn/src"
    ; main_module_name = Some "Dune_spawn"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_stats"
    ; main_module_name = Some "Dune_stats"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "otherlibs/xdg"
    ; main_module_name = Some "Xdg"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "vendor/build_path_prefix_map/src"
    ; main_module_name = Some "Build_path_prefix_map"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "vendor/uutf"
    ; main_module_name = Some "Dune_uutf"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_sexp"
    ; main_module_name = Some "Dune_sexp"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_util"
    ; main_module_name = Some "Dune_util"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_digest"
    ; main_module_name = Some "Dune_digest"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/predicate_lang"
    ; main_module_name = Some "Predicate_lang"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/fiber_util"
    ; main_module_name = Some "Fiber_util"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_cache_storage"
    ; main_module_name = Some "Dune_cache_storage"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_targets"
    ; main_module_name = Some "Dune_targets"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_cache"
    ; main_module_name = Some "Dune_cache"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "otherlibs/ocamlc-loc/src"
    ; main_module_name = Some "Ocamlc_loc"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "otherlibs/dune-rpc/private"
    ; main_module_name = Some "Dune_rpc_private"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "otherlibs/dune-action-plugin/src"
    ; main_module_name = Some "Dune_action_plugin"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_output_truncation"
    ; main_module_name = Some "Dune_output_truncation"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/csexp_rpc"
    ; main_module_name = Some "Csexp_rpc"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_rpc_client"
    ; main_module_name = Some "Dune_rpc_client"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_thread_pool"
    ; main_module_name = Some "Dune_thread_pool"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/fsevents"
    ; main_module_name = Some "Fsevents"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "vendor/ocaml-inotify/src"
    ; main_module_name = Some "Ocaml_inotify"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/async_inotify_for_dune"
    ; main_module_name = Some "Async_inotify_for_dune"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/fswatch_win"
    ; main_module_name = Some "Fswatch_win"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_file_watcher"
    ; main_module_name = Some "Dune_file_watcher"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_engine"
    ; main_module_name = Some "Dune_engine"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/action_ext"
    ; main_module_name = Some "Action_ext"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/promote"
    ; main_module_name = Some "Promote"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/ocaml-config"
    ; main_module_name = Some "Ocaml_config"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/ocaml"
    ; main_module_name = Some "Ocaml"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "vendor/sha"
    ; main_module_name = None
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "vendor/opam/src/core"
    ; main_module_name = None
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "vendor/opam-file-format"
    ; main_module_name = None
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "vendor/opam/src/format"
    ; main_module_name = None
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "otherlibs/dune-private-libs/section"
    ; main_module_name = Some "Dune_section"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_lang"
    ; main_module_name = Some "Dune_lang"
    ; include_subdirs_unqualified = true
    ; special_builtin_support = None
    }
  ; { path = "src/fiber_event_bus"
    ; main_module_name = Some "Fiber_event_bus"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "otherlibs/dune-private-libs/meta_parser"
    ; main_module_name = Some "Dune_meta_parser"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/fs"
    ; main_module_name = Some "Fs"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_findlib"
    ; main_module_name = Some "Dune_findlib"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_vcs"
    ; main_module_name = Some "Dune_vcs"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "otherlibs/dune-build-info/src"
    ; main_module_name = Some "Build_info"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = Some "Build_info_data"
    }
  ; { path = "src/sat"
    ; main_module_name = Some "Sat"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_pkg"
    ; main_module_name = Some "Dune_pkg"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/install"
    ; main_module_name = Some "Install"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_threaded_console"
    ; main_module_name = Some "Dune_threaded_console"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "vendor/lwd/lwd"
    ; main_module_name = None
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "vendor/notty/src"
    ; main_module_name = None
    ; include_subdirs_unqualified = true
    ; special_builtin_support = None
    }
  ; { path = "vendor/notty/src-unix"
    ; main_module_name = None
    ; include_subdirs_unqualified = true
    ; special_builtin_support = None
    }
  ; { path = "vendor/lwd/nottui"
    ; main_module_name = None
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_tui"
    ; main_module_name = Some "Dune_tui"
    ; include_subdirs_unqualified = true
    ; special_builtin_support = None
    }
  ; { path = "src/dune_config_file"
    ; main_module_name = Some "Dune_config_file"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_patch"
    ; main_module_name = Some "Dune_patch"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "otherlibs/dune-site/src/private"
    ; main_module_name = Some "Dune_site_private"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/scheme"
    ; main_module_name = Some "Scheme"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/source"
    ; main_module_name = Some "Source"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_rules"
    ; main_module_name = Some "Dune_rules"
    ; include_subdirs_unqualified = true
    ; special_builtin_support = None
    }
  ; { path = "src/upgrader"
    ; main_module_name = Some "Dune_upgrader"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "vendor/cmdliner/src"
    ; main_module_name = None
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_rpc_server"
    ; main_module_name = Some "Dune_rpc_server"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_rpc_impl"
    ; main_module_name = Some "Dune_rpc_impl"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
  ; { path = "src/dune_rules_rpc"
    ; main_module_name = Some "Dune_rules_rpc"
    ; include_subdirs_unqualified = false
    ; special_builtin_support = None
    }
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
