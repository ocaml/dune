  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > EOF
  $ cat >dune.inc <<EOF
  > (dirs foo)
  > (rule (with-stdout-to foo (echo bar)))
  > EOF
  $ cat >dune <<EOF
  > (include dune.inc)
  > EOF
  $ dune build ./foo
  $ cat _build/default/foo
  bar
