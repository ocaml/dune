Test unmangling of js files

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ mkdir output
  $ cat > output/dune <<EOF
  > (melange.emit
  >  (alias melange)
  >  (module_system commonjs))
  > EOF

Using uppercase produces uppercase artifacts

  $ cat > output/Upper.ml <<EOF
  > print_endline "hello"
  > EOF

  $ output=output/output
  $ dune build $output/Upper.js
  $ node _build/default/$output/Upper.js
  hello

Using lowercase produces uppercase artifacts

  $ cat > output/lower.ml <<EOF
  > print_endline "hello"
  > EOF

  $ dune build @melange
  $ node _build/default/$output/lower.js
  hello
