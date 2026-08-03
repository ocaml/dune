Test unmangling of js files

  $ make_melange_project 3.8 0.1

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (alias mel))
  > EOF

Using uppercase produces uppercase artifacts

  $ cat > Upper.ml <<EOF
  > print_endline "hello"
  > EOF

  $ dune build output/Upper.js
  $ node _build/default/output/Upper.js
  hello

Using lowercase produces uppercase artifacts

  $ cat > lower.ml <<EOF
  > print_endline "hello"
  > EOF

  $ dune build @mel
  $ node _build/default/output/lower.js
  hello
