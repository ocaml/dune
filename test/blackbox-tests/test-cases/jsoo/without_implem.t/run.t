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

JSOO archive rules for libraries without modules are not spuriously invalidated

  $ cat > dune <<EOF
  > (library
  >  (name empty)
  >  (wrapped false)
  >  (modules))
  > (executable
  >  (name main)
  >  (modes js)
  >  (modules main)
  >  (libraries empty))
  > EOF

  $ dune build main.bc.js

main.bc.js should not rebuild

  $ dune build main.bc.js
  $ dune trace cat | jq_dune -r '
  > progMatching("js_of_ocaml")
  > | .target_files[]?
  > | select(endswith(".cma.js"))
  > '
  _build/default/.empty.objs/jsoo/effects=disabled/empty.cma.js

  $ dune build main.bc.js
  $ dune trace cat | jq_dune -r '
  > progMatching("js_of_ocaml")
  > | .target_files[]?
  > | select(endswith(".cma.js"))
  > '
  _build/default/.empty.objs/jsoo/effects=disabled/empty.cma.js

JSOO archives in directory groups are not spuriously invalidated

  $ mkdir nested
  $ cat > dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name grouped)
  >  (wrapped false)
  >  (modules))
  > (executable
  >  (name main)
  >  (modes js)
  >  (modules main)
  >  (libraries grouped))
  > EOF

  $ dune build main.bc.js

main.bc.js should not rebuild

  $ dune build main.bc.js
  $ dune trace cat | jq_dune -r '
  > progMatching("js_of_ocaml")
  > | .target_files[]?
  > | select(endswith(".cma.js"))
  > '
  _build/default/.grouped.objs/jsoo/effects=disabled/grouped.cma.js

  $ dune build main.bc.js
  $ dune trace cat | jq_dune -r '
  > progMatching("js_of_ocaml")
  > | .target_files[]?
  > | select(endswith(".cma.js"))
  > '
  _build/default/.grouped.objs/jsoo/effects=disabled/grouped.cma.js
