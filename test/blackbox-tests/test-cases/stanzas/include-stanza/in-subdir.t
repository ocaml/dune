Test the include stanza inside a subdirectory

  $ make_dune_project 3.13
  $ mkdir a
  $ cat >a/dune <<EOF
  > (include dune.inc)
  > EOF
  $ cat >a/dune.inc <<EOF
  > (rule (with-stdout-to foo (echo "")))
  > EOF

  $ dune build a/foo
