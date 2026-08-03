Directory diff records file deletions.

  $ make_directory_targets_project

  $ mkdir expected
  $ printf 'keep\n' > expected/keep
  $ printf 'delete me\n' > expected/delete

  $ write_directory_diff_keep_rule

  $ dune runtest
  File "dune", lines 5-7, characters 0-56:
  5 | (rule
  6 |  (alias runtest)
  7 |  (action (diff expected actual)))
  Error: File expected/delete should be deleted
  [1]

  $ dune promote

  $ test ! -e expected/delete && echo deleted
  deleted

  $ cat expected/keep
  keep
