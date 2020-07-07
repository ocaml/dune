  $ echo '(lang dune 2.0)' > dune-project
  $ echo '(rule (target b) (deps a) (action (progn (echo "running") (with-stdout-to b (cat a)))))' >> dune

Basic incremental compilation test: we avoid re-running the
action when the file is the same and we re-run it when the file changes.

This test checks, among other things, that on the filesystems with poor
timestamp precision we still correctly rebuild when the file changes,
even if two modifications happen in the same time slot.

  $ echo 1 > a
  $ dune build b
  running
  $ cat _build/default/b
  1
  $ dune build b
  $ cat _build/default/b
  1
  $ echo 2 > a
  $ dune build b
  running
  $ cat _build/default/b
  2
  $ echo 3 > a
  $ dune build b
  running
  $ cat _build/default/b
  3
