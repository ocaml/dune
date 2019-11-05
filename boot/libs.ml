let executables = [ "main" ]

let external_libraries = [ "unix"; "threads.posix" ]

let local_libraries =
  [ ("src/stdune/caml", Some "Dune_caml", false, None)
  ; ("src/stdune", Some "Stdune", false, None)
  ; ("src/dune_lang", Some "Dune_lang", false, None)
  ; ("vendor/incremental-cycles/src", Some "Incremental_cycles", false, None)
  ; ("src/dag", Some "Dag", false, None)
  ; ("src/fiber", Some "Fiber", false, None)
  ; ("src/memo", Some "Memo", false, None)
  ; ("src/xdg", Some "Xdg", false, None)
  ; ("src/dune_cache", Some "Dune_cache", false, None)
  ; ("src/dune_cache_daemon", Some "Dune_cache_daemon", false, None)
  ; ("vendor/re/src", Some "Dune_re", false, None)
  ; ("vendor/opam-file-format/src", None, false, None)
  ; ("otherlibs/dune-glob", Some "Dune_glob", false, None)
  ; ("src/ocaml-config", Some "Ocaml_config", false, None)
  ; ("src/catapult", Some "Catapult", false, None)
  ; ("src/jbuild_support", Some "Jbuild_support", false, None)
  ; ("otherlibs/action-plugin/src", Some "Dune_action_plugin", false, None)
  ; ("src/dune", Some "Dune", true, None)
  ; ("vendor/cmdliner/src", None, false, None)
  ;
  ("otherlibs/build-info/src", Some "Build_info", false,
  Some "build_info_data")
  ]
