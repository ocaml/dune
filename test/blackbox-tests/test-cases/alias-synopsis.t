Dune should be able to run alias with synopsis attached

  $ cat > dune-project << EOF
  > (lang dune 3.18)
  > EOF
  $ cat > dune << EOF
  >  (alias (name foo) (synopsis "Foo alias with synopsis"))
  > EOF

We have an alias "foo" with synopsis
  $ dune build @foo
