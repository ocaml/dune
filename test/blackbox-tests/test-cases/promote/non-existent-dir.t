dune promote should be able to promote into directories that don't exist

  $ make_dune_project 3.22

  $ cat >dune <<EOF
  > (rule
  >  (action
  >   (progn
  >    (with-stdout-to foo (echo baz))
  >    (diff dir/foo foo))))
  > EOF

  $ cat >diff.sh <<'EOF'
  > echo a: $1
  > echo b: $2
  > exit 1
  > EOF

  $ dune build ./foo --diff-command "$SHELL $PWD/diff.sh"
  File "dir/foo", line 1, characters 0-0:
  a: dir/foo
  b: foo
  [1]

  $ dune promote
  Promoting _build/default/foo to dir/foo.
