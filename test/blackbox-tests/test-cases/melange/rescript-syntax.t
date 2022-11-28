Test melange.emit promotion

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias dist)
  >  (entries hello)
  >  (module_system commonjs))
  > EOF

  $ cat > hello.res <<EOF
  > let () = print_endline("hello")
  > EOF

  $ dune build @dist
  $ node ./_build/default/hello.js
  hello
