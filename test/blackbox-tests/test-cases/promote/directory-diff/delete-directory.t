Directory diff records directory deletions.

  $ make_directory_targets_project

  $ mkdir -p expected/stale
  $ printf 'keep\n' > expected/keep
  $ printf 'stale\n' > expected/stale/file

  $ write_directory_diff_keep_rule

  $ dune runtest
  File "dune", lines 5-7, characters 0-56:
  5 | (rule
  6 |  (alias runtest)
  7 |  (action (diff expected actual)))
  Error: Directory expected/stale should be deleted
  [1]

  $ dune promote

  $ test ! -d expected/stale && echo deleted
  deleted

  $ cat expected/keep
  keep
