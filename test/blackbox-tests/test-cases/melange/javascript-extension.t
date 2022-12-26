Test (javascript_extension) field on melange.emit stanzas

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ cat > hello.ml <<EOF
  > let () =
  >   print_endline "hello"
  > EOF

Can use extension with dots

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias melange)
  >  (module_system commonjs)
  >  (javascript_extension bs.js))
  > EOF

  $ dune build @melange
  $ node _build/default/output/hello.bs.js
  hello

Errors out if extension starts with dot

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias melange)
  >  (module_system commonjs)
  >  (javascript_extension .bs.js))
  > EOF

  $ dune build @melange
  File "dune", line 5, characters 23-29:
  5 |  (javascript_extension .bs.js))
                             ^^^^^^
  Error: extension must not start with '.'
  [1]
