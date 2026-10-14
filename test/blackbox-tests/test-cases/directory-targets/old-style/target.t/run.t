Old-style directory targets are rejected, whether built directly or as a
dependency of another action.

  $ dune build
  File "dune", lines 1-3, characters 0-70:
  1 | (rule
  2 |  (targets dir)
  3 |  (action (run dune_cmd make-dir-with-files dir)))
  Error: Error trying to read targets after a rule was run:
  - dir: Directory produced for a file target. Use (dir dir).
  [1]

  $ dune build @cat_dir
  File "dune", lines 1-3, characters 0-70:
  1 | (rule
  2 |  (targets dir)
  3 |  (action (run dune_cmd make-dir-with-files dir)))
  Error: Error trying to read targets after a rule was run:
  - dir: Directory produced for a file target. Use (dir dir).
  [1]
