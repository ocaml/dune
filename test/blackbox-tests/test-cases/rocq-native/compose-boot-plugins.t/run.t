Testing composition with a boot library with plugins

  $ dune build
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
