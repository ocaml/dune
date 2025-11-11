Test multiple module systems with promotion in-source

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using melange 1.0)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (emit_stdlib false)
  >  (promote (until-clean))
  >  (module_systems
  >   (commonjs mel.js)
  >   (esm mjs)))
  > EOF
  $ cat > main.ml <<EOF
  > let () = Js.log "hello"
  > EOF

  $ dune build @mel

Outputs both module systems

  $ ls _build/default/output
  main.mel.js
  main.mjs
  $ node _build/default/output/main.mel.js
  hello
  $ node _build/default/output/main.mjs
  hello

  $ find . -type f -not -path "./_build/*" | sort
  ./dune
  ./dune-project
  ./main.mel.js
  ./main.mjs
  ./main.ml

  $ dune clean
  $ cat > dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (emit_stdlib false)
  >  (promote
  >   (until-clean)
  >   (into ./output))
  >  (module_systems
  >   (commonjs mel.js)
  >   (esm mjs)))
  > EOF
  $ cat > main.ml <<EOF
  > let () = Js.log "hello"
  > EOF

  $ dune build @mel

  $ find . -type f -not -path "./_build/*" | sort
  ./dune
  ./dune-project
  ./main.ml
  ./output/main.mel.js
  ./output/main.mjs
