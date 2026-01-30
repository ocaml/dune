Normal include from a dynamic include

  $ make_dune_project 3.14

  $ mkdir a b
  $ cat >a/dune <<EOF
  > (dynamic_include ../b/dune.inc)
  > EOF

  $ cat >b/dune.inc <<EOF
  > (include ../b/dune2.inc)
  > EOF

  $ cat >b/dune2.inc <<EOF
  > (rule (with-stdout-to foo (echo bar)))
  > EOF

  $ dune build a/foo
