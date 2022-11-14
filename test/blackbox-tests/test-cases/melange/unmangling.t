Test unmangling of js files

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (module_system commonjs))
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

  $ dune build output/lower.js
  $ node _build/default/output/lower.js
  hello
