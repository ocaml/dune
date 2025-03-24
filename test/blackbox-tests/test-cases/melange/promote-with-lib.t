Test melange.emit promotion

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
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
  >  (modules hello)
  >  (promote (until-clean))
  >  (emit_stdlib false)
  >  (target dist)
  >  (libraries mylib))
  > EOF

  $ cat > hello.ml <<EOF
  > let the_binding = Mylib.some_binding
  > let () =
  >   print_endline "hello"
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
