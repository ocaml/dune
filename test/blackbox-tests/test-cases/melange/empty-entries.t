Test (modules) field can be left empty

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias mel))
  > EOF

  $ cat > hello.ml <<EOF
  > let () =
  >   print_endline "hello"
  > EOF

  $ dune build @mel
  $ node _build/default/output/hello.js
  hello
