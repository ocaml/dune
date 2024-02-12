Demonstrate that we can load dynamically generated rules

  $ cat >dune-project <<EOF
  > (lang dune 3.14)
  > EOF

  $ mkdir a b

  $ cat >a/dune <<EOF
  > (rule
  >  (with-stdout-to dune.inc
  >   (echo "(rule (with-stdout-to foo (echo dynamic)))")))
  > EOF

  $ dune build a/dune.inc

  $ cat >b/dune <<EOF
  > (dynamic_include ../a/dune.inc)
  > EOF

  $ dune build b/foo
