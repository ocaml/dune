Test the include stanza inside a subdirectory

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF
  $ mkdir a
  $ cat >a/dune <<EOF
  > (include dune.inc)
  > EOF
  $ cat >a/dune.inc <<EOF
  > (rule (with-stdout-to foo (echo "")))
  > EOF

  $ dune build a/foo
