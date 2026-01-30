Test the include stanza pulling from a parent dir

  $ make_dune_project 3.13
  $ mkdir a
  $ cat >a/dune <<EOF
  > (include ../dune.inc)
  > EOF
  $ cat >dune.inc <<EOF
  > (rule (with-stdout-to foo (echo "")))
  > EOF

  $ dune build a/foo
