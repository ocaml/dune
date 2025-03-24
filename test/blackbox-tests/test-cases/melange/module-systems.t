Parses the full form (<module-system> <extension>)

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (emit_stdlib false)
  >  (module_systems
  >   (commonjs bs.js)
  >   (es6 mjs)))
  > EOF
  $ cat > main.ml <<EOF
  > let () = Js.log "hello"
  > EOF

  $ dune build @mel --display=short
          melc .output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc output/main.bs.js
          melc output/main.mjs

Outputs both module systems

  $ ls _build/default/output
  main.bs.js
  main.mjs
  $ node _build/default/output/main.bs.js
  hello
  $ node _build/default/output/main.mjs
  hello


Parses the simplified form and defaults extension to `.js`

  $ dune clean
  $ cat > dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (emit_stdlib false)
  >  (module_systems
  >   commonjs
  >   (es6 mjs)))
  > EOF
  $ cat > main.ml <<EOF
  > let () = Js.log "hello"
  > EOF

  $ dune build @mel --display=short
          melc .output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc output/main.js
          melc output/main.mjs

  $ ls _build/default/output
  main.js
  main.mjs

Accepts `"esm"` too for ESM modules

  $ dune clean
  $ cat > dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (emit_stdlib false)
  >  (module_systems commonjs (esm mjs)))
  > EOF

  $ dune build @mel
  $ ls _build/default/output
  main.js
  main.mjs

Defaults to commonjs / `.js` if no config present at all

  $ dune clean
  $ cat > dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (emit_stdlib false)
  >  (target output))
  > EOF
  $ cat > main.ml <<EOF
  > let () = Js.log "hello"
  > EOF

  $ dune build @mel --display=short
          melc .output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc output/main.js

  $ ls _build/default/output
  main.js

Errors out if extension starts with dot

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (emit_stdlib false)
  >  (module_systems (commonjs .bs.js)))
  > EOF

  $ dune build @mel --display=short
  File "dune", line 5, characters 27-33:
  5 |  (module_systems (commonjs .bs.js)))
                                 ^^^^^^
  Error: extension must not start with '.'
  [1]

Errors if the same extension is present multiple times

  $ dune clean
  $ cat > dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (emit_stdlib false)
  >  (module_systems commonjs es6))
  > EOF
  $ cat > main.ml <<EOF
  > let () = Js.log "hello"
  > EOF

  $ dune build @mel --display=short
  File "dune", line 5, characters 26-29:
  5 |  (module_systems commonjs es6))
                                ^^^
  Error: JavaScript extension .js appears more than once:
  - dune:5
  - dune:5
  Extensions must be unique per melange.emit stanza
  Hint: specify different extensions with (module_systems (<system1>
  <extension1>) (<system2> <extension2>))
  [1]

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (emit_stdlib false)
  >  (module_systems (commonjs bs.js) commonjs (es6 bs.js)))
  > EOF
  $ cat > main.ml <<EOF
  > let () = Js.log "hello"
  > EOF

  $ dune build @mel --display=short
  File "dune", line 5, characters 48-53:
  5 |  (module_systems (commonjs bs.js) commonjs (es6 bs.js)))
                                                      ^^^^^
  Error: JavaScript extension .bs.js appears more than once:
  - dune:5
  - dune:5
  Extensions must be unique per melange.emit stanza
  Hint: specify different extensions with (module_systems (<system1>
  <extension1>) (<system2> <extension2>))
  [1]
