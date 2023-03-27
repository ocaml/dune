Test alias field on melange.emit stanzas

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias app))
  > EOF

  $ cat > main.ml <<EOF
  > let () =
  >   print_endline "hello"
  > EOF

  $ dune build @app
  $ node _build/default/output/main.js
  hello

Default alias melange works

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (modules main))
  > (melange.emit
  >  (target output2)
  >  (modules main2))
  > EOF

  $ cat > main2.ml <<EOF
  > let () =
  >   print_endline "hello"
  > EOF

  $ dune clean
  $ dune build @melange
  $ node _build/default/output/main.js
  hello
  $ node _build/default/output2/main2.js
  hello

Users can override melange alias (even if useless)

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias melange))
  > EOF

  $ dune clean
  $ dune build @melange

If user defines an alias, the default alias is not used for that stanza 

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias foo))
  > EOF

  $ dune clean
  $ dune build @melange
  Error: Alias "melange" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  [1]
