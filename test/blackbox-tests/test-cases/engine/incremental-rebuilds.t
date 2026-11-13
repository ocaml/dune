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

A multi-target cache entry must not survive another rule taking over one of its
outputs. Restore the original rule and check both outputs, then its null build.

  $ export DUNE_CACHE=disabled
  $ write_multi_rule () {
  > cat > dune <<'EOF'
  > (rule
  >  (targets owner-a owner-b)
  >  (action (bash "echo multi; echo one > owner-a; echo two > owner-b")))
  > EOF
  > }
  $ write_multi_rule
  $ dune build owner-a
  multi

  $ cat > dune <<'EOF'
  > (rule (target owner-a) (action (write-file owner-a one)))
  > (rule
  >  (target owner-b)
  >  (action (bash "echo single; echo changed > owner-b")))
  > EOF
  $ dune build owner-b
  single

  $ write_multi_rule
  $ dune build owner-a
  multi
  $ cat _build/default/owner-a _build/default/owner-b
  one
  two
  $ dune build owner-a owner-b
