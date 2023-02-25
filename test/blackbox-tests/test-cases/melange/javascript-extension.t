Test (javascript_extension) field on melange.emit stanzas

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ mkdir output
  $ cat > output/hello.ml <<EOF
  > let () =
  >   print_endline "hello"
  > EOF

Can use extension with dots

  $ cat > output/dune <<EOF
  > (melange.emit
  >  (alias melange)
  >  (module_system commonjs)
  >  (javascript_extension bs.js))
  > EOF

  $ dune build @melange
  $ node _build/default/output/output/hello.bs.js
  hello

Errors out if extension starts with dot

  $ cat > output/dune <<EOF
  > (melange.emit
  >  (alias melange)
  >  (module_system commonjs)
  >  (javascript_extension .bs.js))
  > EOF

  $ dune build @melange
  File "output/dune", line 4, characters 23-29:
  4 |  (javascript_extension .bs.js))
                             ^^^^^^
  Error: extension must not start with '.'
  [1]
