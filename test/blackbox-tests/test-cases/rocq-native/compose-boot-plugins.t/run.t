Testing composition with a boot library with plugins

  $ dune build
  File "./Coq/theories/Init/Prelude.v", line 1, characters 0-53:
  Warning:
  Legacy loading plugin method has been removed from Rocq, and the `:` syntax is deprecated, and its first argument ignored; please remove "boot_plugin:" from your Declare ML
  [legacy-loading-removed,deprecated-since-9.0,deprecated,default]
  plugin loaded
  File "Coq/theories/dune", lines 1-5, characters 0-85:
  1 | (rocq.theory
  2 |  (name Coq)
  3 |  (boot)
  4 |  (package coq-boot)
  5 |  (plugins coq-boot.boot_plugin))
  Error: Rule failed to generate the following targets:
  - Coq/theories/Init/NCoq_Init_Prelude.cmi
  - Coq/theories/Init/NCoq_Init_Prelude.cmxs
  [1]
