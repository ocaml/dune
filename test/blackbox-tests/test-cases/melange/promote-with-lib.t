Test melange.emit promotion

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (modes melange)
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
  >  (libraries mylib)
  >  (module_system commonjs))
  > EOF

  $ cat > hello.ml <<EOF
  > let the_binding = Mylib.some_binding
  > let () =
  >   print_endline "hello"
  > EOF

  $ dune build @dist

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
