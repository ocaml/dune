open Types
let external_libraries = [ "unix"; "threads" ]

let local_libraries =
  [ { path = "otherlibs/top-closure"
    ; main_module_name = Some "Top_closure"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "otherlibs/ordering"
    ; main_module_name = Some "Ordering"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "otherlibs/dyn"
    ; main_module_name = Some "Dyn"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "otherlibs/fs-io"
    ; main_module_name = Some "Fs_io"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "otherlibs/stdune/src"
    ; main_module_name = Some "Stdune"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module =
        Some
          { name = "Root"
          ; entries =
              [ "Unix"
              ; "UnixLabels"
              ; "Csexp"
              ; "Top_closure"
              ; "Ordering"
              ; "Dyn"
              ; "Pp"
              ; "Fs_io"
              ]
          }
    }
  ; { path = "src/dune_graph"
    ; main_module_name = Some "Dune_graph"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "vendor/incremental-cycles/src"
    ; main_module_name = Some "Incremental_cycles"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dag"
    ; main_module_name = Some "Dag"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/fiber/src"
    ; main_module_name = Some "Fiber"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_console"
    ; main_module_name = Some "Dune_console"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/memo"
    ; main_module_name = Some "Memo"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_config"
    ; main_module_name = Some "Dune_config"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "otherlibs/dune-glob/src"
    ; main_module_name = Some "Dune_glob"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_metrics"
    ; main_module_name = Some "Dune_metrics"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "otherlibs/dune-action-trace"
    ; main_module_name = Some "Dune_action_trace"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "vendor/bigstringaf"
    ; main_module_name = Some "Bigstringaf"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_trace"
    ; main_module_name = Some "Dune_trace"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "otherlibs/xdg"
    ; main_module_name = Some "Xdg"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "vendor/build_path_prefix_map/src"
    ; main_module_name = Some "Build_path_prefix_map"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_sexp"
    ; main_module_name = Some "Dune_sexp"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_util"
    ; main_module_name = Some "Dune_util"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/fsevents"
    ; main_module_name = Some "Fsevents"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "vendor/ocaml-inotify/src"
    ; main_module_name = Some "Ocaml_inotify"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/async_inotify_for_dune"
    ; main_module_name = Some "Async_inotify_for_dune"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/fswatch_win"
    ; main_module_name = Some "Fswatch_win"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_file_watcher"
    ; main_module_name = Some "Dune_file_watcher"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_scheduler"
    ; main_module_name = Some "Dune_scheduler"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "vendor/ocaml-blake3-mini"
    ; main_module_name = Some "Blake3_mini"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_digest"
    ; main_module_name = Some "Dune_digest"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/predicate_lang"
    ; main_module_name = Some "Predicate_lang"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/fiber_util"
    ; main_module_name = Some "Fiber_util"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_cache_storage"
    ; main_module_name = Some "Dune_cache_storage"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_targets"
    ; main_module_name = Some "Dune_targets"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_cache"
    ; main_module_name = Some "Dune_cache"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "otherlibs/chrome-trace/src"
    ; main_module_name = Some "Chrome_trace"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "otherlibs/ocamlc-loc/src"
    ; main_module_name = Some "Ocamlc_loc"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "otherlibs/dune-rpc/private"
    ; main_module_name = Some "Dune_rpc_private"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "otherlibs/dune-action-plugin/src"
    ; main_module_name = Some "Dune_action_plugin"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/csexp_rpc"
    ; main_module_name = Some "Csexp_rpc"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_rpc_client"
    ; main_module_name = Some "Dune_rpc_client"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_engine"
    ; main_module_name = Some "Dune_engine"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/action_ext"
    ; main_module_name = Some "Action_ext"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/promote"
    ; main_module_name = Some "Promote"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/ocaml-config"
    ; main_module_name = Some "Ocaml_config"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/ocaml"
    ; main_module_name = Some "Ocaml"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "vendor/sha"
    ; main_module_name = None
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "vendor/opam/src/core"
    ; main_module_name = None
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "vendor/opam-file-format"
    ; main_module_name = None
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "vendor/opam/src/format"
    ; main_module_name = None
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "otherlibs/dune-private-libs/section"
    ; main_module_name = Some "Dune_section"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_lang"
    ; main_module_name = Some "Dune_lang"
    ; include_subdirs = Unqualified
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/fiber_event_bus"
    ; main_module_name = Some "Fiber_event_bus"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "otherlibs/dune-private-libs/meta_parser"
    ; main_module_name = Some "Dune_meta_parser"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/fs"
    ; main_module_name = Some "Fs"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_findlib"
    ; main_module_name = Some "Dune_findlib"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "vendor/ocaml-lmdb"
    ; main_module_name = Some "Lmdb"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_vcs"
    ; main_module_name = Some "Dune_vcs"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "otherlibs/dune-build-info/src"
    ; main_module_name = Some "Build_info"
    ; include_subdirs = No
    ; special_builtin_support = Some "Build_info_data"
    ; root_module = None
    }
  ; { path = "src/sat"
    ; main_module_name = Some "Sat"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_pkg"
    ; main_module_name = Some "Dune_pkg"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/install"
    ; main_module_name = Some "Install"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/action_plugin"
    ; main_module_name = Some "Action_plugin"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_threaded_console"
    ; main_module_name = Some "Dune_threaded_console"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "vendor/lwd/lwd"
    ; main_module_name = None
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "vendor/notty/src"
    ; main_module_name = None
    ; include_subdirs = Unqualified
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "vendor/notty/src-unix"
    ; main_module_name = None
    ; include_subdirs = Unqualified
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "vendor/lwd/nottui"
    ; main_module_name = None
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_tui"
    ; main_module_name = Some "Dune_tui"
    ; include_subdirs = Unqualified
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_config_file"
    ; main_module_name = Some "Dune_config_file"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_patch"
    ; main_module_name = Some "Dune_patch"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "otherlibs/dune-site/src/private"
    ; main_module_name = Some "Dune_site_private"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/scheme"
    ; main_module_name = Some "Scheme"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/source"
    ; main_module_name = Some "Source"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_rules"
    ; main_module_name = Some "Dune_rules"
    ; include_subdirs = Unqualified
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/upgrader"
    ; main_module_name = Some "Dune_upgrader"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "vendor/cmdliner/src"
    ; main_module_name = None
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_rpc_server"
    ; main_module_name = Some "Dune_rpc_server"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ; { path = "src/dune_rpc_impl"
    ; main_module_name = Some "Dune_rpc_impl"
    ; include_subdirs = No
    ; special_builtin_support = None
    ; root_module = None
    }
  ]

let main =
  { path = "bin"
  ; main_module_name = None
  ; include_subdirs = Qualified
  ; special_builtin_support = None
  ; root_module =
      Some
        { name = "Root"
        ; entries =
            [ "Memo"
            ; "Promote"
            ; "Ocaml"
            ; "Ocaml_config"
            ; "Dune_sexp"
            ; "Dune_lang"
            ; "Predicate_lang"
            ; "Fiber"
            ; "Fiber_event_bus"
            ; "Top_closure"
            ; "Ordering"
            ; "Dyn"
            ; "Pp"
            ; "Stdune"
            ; "Fs_io"
            ; "Dune_console"
            ; "Unix"
            ; "UnixLabels"
            ; "Install"
            ; "Dune_findlib"
            ; "Dune_metrics"
            ; "Dune_digest"
            ; "Dune_cache"
            ; "Dune_cache_storage"
            ; "Dune_graph"
            ; "Dune_rules"
            ; "Dune_vcs"
            ; "Dune_engine"
            ; "Dune_scheduler"
            ; "Dune_targets"
            ; "Dune_util"
            ; "Dune_upgrader"
            ; "Dune_pkg"
            ; "Cmdliner"
            ; "Cmdliner_arg"
            ; "Cmdliner_base"
            ; "Cmdliner_cline"
            ; "Cmdliner_cmd"
            ; "Cmdliner_docgen"
            ; "Cmdliner_eval"
            ; "Cmdliner_info"
            ; "Cmdliner_manpage"
            ; "Cmdliner_msg"
            ; "Cmdliner_term"
            ; "Cmdliner_term_deprecated"
            ; "Cmdliner_trie"
            ; "Build_info"
            ; "Dune_config"
            ; "Dune_config_file"
            ; "Chrome_trace"
            ; "Dune_trace"
            ; "Csexp"
            ; "Csexp_rpc"
            ; "Dune_rpc_impl"
            ; "Dune_rpc_private"
            ; "Dune_rpc_client"
            ; "Spawn"
            ; "OpamCompat"
            ; "OpamConsole"
            ; "OpamCoreConfig"
            ; "OpamCoreConfigDeveloper"
            ; "OpamDirTrack"
            ; "OpamFilename"
            ; "OpamHash"
            ; "OpamJson"
            ; "OpamSHA"
            ; "OpamSWHID"
            ; "OpamStd"
            ; "OpamStubs"
            ; "OpamStubsTypes"
            ; "OpamSystem"
            ; "OpamUrl"
            ; "OpamVersion"
            ; "OpamVersionCompare"
            ; "OpamVersionInfo"
            ; "OpamBaseParser"
            ; "OpamLexer"
            ; "OpamParser"
            ; "OpamParserTypes"
            ; "OpamPrinter"
            ; "OpamFile"
            ; "OpamFilter"
            ; "OpamFormat"
            ; "OpamFormatConfig"
            ; "OpamFormula"
            ; "OpamInterpLexer"
            ; "OpamLineLexer"
            ; "OpamPackage"
            ; "OpamPp"
            ; "OpamRepositoryName"
            ; "OpamSwitch"
            ; "OpamSysPkg"
            ; "OpamTypes"
            ; "OpamTypesBase"
            ; "OpamVariable"
            ; "Source"
            ; "Xdg"
            ; "Re"
            ]
        }
  }

