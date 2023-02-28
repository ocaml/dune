Test melange.emit promotion

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (modes :standard melange)
  >  (name mylib))
  > EOF

  $ cat > lib/mylib.ml <<EOF
  > let some_binding = Some "string"
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias dist)
  >  (entries hello)
  >  (promote (until-clean))
  >  (target dist)
  >  (libraries mylib))
  > EOF

  $ cat > hello.ml <<EOF
  > let the_binding = Mylib.some_binding
  > let () =
  >   print_endline "hello"
  > EOF

Fails with an informative error message if we parsed OSL for modes

  $ dune build @dist
  File "lib/dune", line 1, characters 0-50:
  1 | (library
  2 |  (modes :standard melange)
  3 |  (name mylib))
  Error: Ordered set language for modes is only available since version 3.8 of
  the dune language. Please update your dune-project file to have (lang dune
  3.8).
  [1]


  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF
  $ dune build @dist

Library has `(modes :standard)` so it also builds for bytecode / native

  $ dune build lib/.mylib.objs/byte/mylib.cmo
  $ dune build lib/.mylib.objs/native/mylib.cmx

Targets are promoted to the source tree

  $ ls ./dist
  hello.js
  lib
  $ ls ./dist/lib
  mylib.js

  $ node ./dist/hello.js
  hello

(until-clean) causes JS file targets to be deleted after calling dune clean

  $ dune clean
  $ ls ./dist
  lib
  $ ls ./dist/lib
