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

JSOO rules for executables are not spuriously invalidated

  $ dune build
  $ dune trace cat | jq_dune -r '
  > progMatching("js_of_ocaml")
  > | .target_files[]?
  > | select(endswith("main.bc.js"))
  > '

Source directories that resemble library object directories are not reserved
for JSOO rules.

  $ mkdir .foo.objs
  $ cat > dune <<EOF
  > (dirs :standard .foo.objs)
  > EOF
  $ cat > .foo.objs/dune <<EOF
  > (rule
  >  (target jsoo)
  >  (action (with-stdout-to %{target} (echo source))))
  > EOF

  $ dune build .foo.objs/jsoo
  $ cat _build/default/.foo.objs/jsoo
  source

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

  $ dune build --display=short main.bc.js

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

  $ dune build main.bc.js
  $ dune trace cat | jq_dune -r '
  > progMatching("js_of_ocaml")
  > | .target_files[]?
  > | select(endswith(".cma.js"))
  > '

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

  $ dune build main.bc.js
  $ dune trace cat | jq_dune -r '
  > progMatching("js_of_ocaml")
  > | .target_files[]?
  > | select(endswith(".cma.js"))
  > '
