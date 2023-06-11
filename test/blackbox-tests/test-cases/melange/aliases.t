Test alias field on melange.emit stanzas

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (alias app))
  > EOF

  $ cat > main.ml <<EOF
  > let () =
  >   Js.log "hello"
  > EOF

  $ dune build @app
  $ node _build/default/output/main.js
  hello

Default alias melange works

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (modules main))
  > (melange.emit
  >  (target output2)
  >  (emit_stdlib false)
  >  (modules main2))
  > EOF

  $ cat > main2.ml <<EOF
  > let () =
  >   Js.log "hello"
  > EOF

  $ dune clean
  $ dune build @melange
  $ node _build/default/output/main.js
  hello
  $ node _build/default/output2/main2.js
  hello

Dune default alias works

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (modules main)
  >  (emit_stdlib false))
  > (melange.emit
  >  (target output2)
  >  (modules main2)
  >  (emit_stdlib false))
  > EOF

  $ cat > main2.ml <<EOF
  > let () =
  >   Js.log "hello"
  > EOF

  $ dune clean
  $ dune build
  $ node _build/default/output/main.js
  hello
  $ node _build/default/output2/main2.js
  hello

Users can override melange alias (even if useless)

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (alias melange))
  > EOF

  $ dune clean
  $ dune build @melange

If user defines an alias, the default alias is not used for that stanza

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (alias foo))
  > EOF

  $ dune clean
  $ dune build @melange
  Error: Alias "melange" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  [1]

Even if user defines an alias, dune default alias should still work

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (modules main)
  >  (emit_stdlib false)
  >  (alias foo))
  > EOF

  $ dune clean
  $ dune build
  $ node _build/default/output/main.js
  hello
