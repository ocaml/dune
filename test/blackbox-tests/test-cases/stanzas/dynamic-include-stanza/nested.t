Nesting of dynamic_include stanzas

  $ mkdir a b c

  $ cat >dune-project <<EOF
  > (lang dune 3.14)
  > EOF

  $ cat >a/dune <<EOF
  > (dynamic_include ../b/dune.inc)
  > EOF

  $ cat >b/dune <<EOF
  > (rule
  >  (with-stdout-to dune.inc
  >   (echo "(dynamic_include ../c/dune.inc)")))
  > EOF

  $ cat >c/dune <<EOF
  > (rule
  >  (with-stdout-to dune.inc
  >   (echo "(rule (with-stdout-to foo (echo bar)))")))
  > EOF

  $ dune build a/foo
