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
  File ".main.eobjs/jsoo/_unknown_", line 1, characters 0-0:
  Error: No rule found for .main.eobjs/jsoo/dune__exe__Interface.cmo.js
  [1]
