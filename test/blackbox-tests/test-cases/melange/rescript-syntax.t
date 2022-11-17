Test melange.emit promotion

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias dist)
  >  (entries hello)
  >  (target dist)
  >  (module_system commonjs))
  > EOF

  $ cat > hello.res <<EOF
  > let () = print_endline("hello")
  > EOF

  $ dune build @dist
  $ node ./_build/default/dist/hello.js
  hello
