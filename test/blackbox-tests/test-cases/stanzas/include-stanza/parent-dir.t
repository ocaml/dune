Test the include stanza pulling from a parent dir

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF
  $ mkdir a
  $ cat >a/dune <<EOF
  > (include ../dune.inc)
  > EOF
  $ cat >dune.inc <<EOF
  > (rule (with-stdout-to foo (echo "")))
  > EOF

  $ dune build a/foo
