Test melange.emit promotion with public library dependencies

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (package (name foo))
  > (using melange 0.1)
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (public_name foo)
  >  (modes melange))
  > EOF

  $ cat > lib/foo.ml <<EOF
  > let some_binding = Some "string"
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias dist)
  >  (modules hello)
  >  (promote (until-clean))
  >  (emit_stdlib false)
  >  (target dist)
  >  (libraries foo))
  > EOF

  $ cat > hello.ml <<EOF
  > let the_binding = Foo.some_binding
  > let () = print_endline "hello"
  > EOF

  $ dune build @dist

Targets are promoted to the source tree

  $ ls ./dist
  hello.js
  node_modules
  $ ls ./dist/node_modules/foo
  foo.js

  $ node ./dist/hello.js
  hello

(until-clean) causes JS file targets to be deleted after calling dune clean

  $ dune clean
  $ ls ./dist
  node_modules
  $ ls ./dist/node_modules/foo
