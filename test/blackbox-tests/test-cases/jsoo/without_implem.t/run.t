Tests JSOO rules for modules without implementations.

  $ make_dune_project 3.21

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

JSOO archive rules for interface-only libraries are not spuriously invalidated

  $ cat > dune <<EOF
  > (library
  >  (name interface)
  >  (modules interface)
  >  (modules_without_implementation interface))
  > 
  > (executable
  >  (name main)
  >  (modes js)
  >  (modules main)
  >  (libraries interface))
  > EOF

  $ cat > main.ml <<EOF
  > let () = ()
  > EOF

  $ dune build main.bc.js

main.bc.js should not rebuild

  $ dune build --display=short main.bc.js
   js_of_ocaml .interface.objs/jsoo/effects=disabled/interface.cma.js

  $ dune build --display=short main.bc.js
   js_of_ocaml .interface.objs/jsoo/effects=disabled/interface.cma.js

