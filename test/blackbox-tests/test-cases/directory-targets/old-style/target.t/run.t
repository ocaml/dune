Tests old-style directory targets as explicit targets.

  $ dune build && cat _build/default/dir/*
  File "dune", lines 11-13, characters 0-51:
  11 | (rule
  12 |  (targets dir)
  13 |  (action (run ./foo.exe dir)))
  Error: Error trying to read targets after a rule was run:
  - dir: Unexpected file kind "S_DIR" (directory)
  [1]

  $ dune build @cat_dir
  File "dune", lines 11-13, characters 0-51:
  11 | (rule
  12 |  (targets dir)
  13 |  (action (run ./foo.exe dir)))
  Error: Error trying to read targets after a rule was run:
  - dir: Unexpected file kind "S_DIR" (directory)
  [1]

