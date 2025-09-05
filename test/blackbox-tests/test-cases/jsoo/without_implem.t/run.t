  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (modes js)
  >  (modules_without_implementation interface))
  > EOF

  $ cat > main.ml <<EOF
  > let _ = Interface.Foo
  > EOF

  $ cat > interface.mli <<EOF
  > type t = Foo
  > EOF

  $ dune build
