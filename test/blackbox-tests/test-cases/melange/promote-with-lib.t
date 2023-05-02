Test melange.emit promotion

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
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
  > (executable
  >  (name hello))
  > EOF

  $ cat > hello.ml <<EOF
  > let the_binding = Mylib.some_binding
  > let () =
  >   print_endline "hello"
  > EOF

Fails with an informative error message if we parsed OSL for modes

  $ dune build hello.exe
  File "lib/dune", line 2, characters 8-17:
  2 |  (modes :standard melange)
              ^^^^^^^^^
  Error: Unknown value :standard
  [1]


  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias dist)
  >  (modules hello)
  >  (promote (until-clean))
  >  (target dist)
  >  (libraries mylib))
  > EOF

  $ dune build @dist
Library has `(modes :standard)` so it also builds for bytecode / native

  $ dune build lib/.mylib.objs/byte/mylib.cmo
  $ dune build lib/.mylib.objs/native/mylib.cmx

Targets are promoted to the source tree

  $ ls ./dist
  hello.js
  lib
  node_modules
  $ ls ./dist/lib
  mylib.js

  $ node ./dist/hello.js
  hello

(until-clean) causes JS file targets to be deleted after calling dune clean

  $ dune clean
  $ ls ./dist
  lib
  node_modules
  $ ls ./dist/lib
