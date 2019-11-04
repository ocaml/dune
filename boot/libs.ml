let external_libraries = [ "unix"; "threads.posix" ]

let local_libraries =
  [ ("src/stdune/caml", Some "Dune_caml", No)
  ; ("src/stdune", Some "Stdune", No)
  ; ("src/dune_lang", Some "Dune_lang", No)
  ; ("vendor/incremental-cycles/src", Some "Incremental_cycles", No)
  ; ("src/dag", Some "Dag", No)
  ; ("src/fiber", Some "Fiber", No)
  ; ("src/memo", Some "Memo", No)
  ; ("src/xdg", Some "Xdg", No)
  ; ("src/dune_cache", Some "Dune_cache", No)
  ; ("src/dune_cache_daemon", Some "Dune_cache_daemon", No)
  ; ("vendor/re/src", Some "Dune_re", No)
  ; ("vendor/opam-file-format/src", None, No)
  ; ("otherlibs/dune-glob", Some "Dune_glob", No)
  ; ("src/ocaml-config", Some "Ocaml_config", No)
  ; ("src/catapult", Some "Catapult", No)
  ; ("src/jbuild_support", Some "Jbuild_support", No)
  ; ("otherlibs/action-plugin/src", Some "Dune_action_plugin", No)
  ; ("src/dune", Some "Dune", Unqualified)
  ; ("vendor/cmdliner/src", None, No)
  ; ("otherlibs/build-info/src", Some "Build_info", No)
  ]
