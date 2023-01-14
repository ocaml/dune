Test (entries) field can be left empty

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias melange)
  >  (module_system commonjs))
  > EOF

  $ cat > hello.ml <<EOF
  > let () =
  >   print_endline "hello"
  > EOF

  $ dune build @melange
  $ node _build/default/output/hello.js
  hello
