Test source-backed artifacts produced by promotion in file-watching mode.

A promoted file should be available through the corresponding source-copy target
in _build.

  $ mkdir promoted-source-copy
  $ cd promoted-source-copy
  $ write_copy_promotion_project
  $ echo hi > original
  $ start_dune
  $ build result
  Success
  $ cat promoted
  hi
  $ cat _build/default/promoted
  hi
  $ cat _build/default/result
  hi
  hi
  $ echo bye > original
  $ build result
  Success
  $ cat promoted
  bye
  $ cat _build/default/promoted
  bye
  $ cat _build/default/result
  bye
  bye
  $ stop_dune > /dev/null
  $ cd ..

Current behavior: promotion into an existing source path suppresses the file
watcher event for that source path. The source file is updated, but an existing
source-copy target depending on it remains stale after that promotion. Watch
mode may execute the source-copy rule one or more times around the promotion,
so the test avoids asserting the exact run log and only checks that promotion
happened.

  $ mkdir promote-and-source-copy
  $ cd promote-and-source-copy
  $ printf old > data
  $ runs="$PWD/runs"
  $ write_promote_and_source_copy_project "$runs"
  $ start_dune @idle
  $ build result
  Success
  $ cat _build/default/result
  old
  $ build sub/data
  Success
  $ with_timeout dune rpc flush-file-watcher --wait
  $ cat data
  new
  $ build result
  Success
  $ cat _build/default/result
  old
  $ if grep -q '^promote$' runs; then echo promote; else echo missing; fi
  promote
  $ stop_dune > /dev/null
