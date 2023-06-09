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
  node:internal/modules/cjs/loader:1078
    throw err;
    ^
  
  Error: Cannot find module '$TESTCASE_ROOT/_build/default/output/main.js'
      at Module._resolveFilename (node:internal/modules/cjs/loader:1075:15)
      at Module._load (node:internal/modules/cjs/loader:920:27)
      at Function.executeUserEntryPoint [as runMain] (node:internal/modules/run_main:81:12)
      at node:internal/main/run_main_module:23:47 {
    code: 'MODULE_NOT_FOUND',
    requireStack: []
  }
  
  Node.js v18.16.0
  [1]
  $ node _build/default/output2/main2.js
  node:internal/modules/cjs/loader:1078
    throw err;
    ^
  
  Error: Cannot find module '$TESTCASE_ROOT/_build/default/output2/main2.js'
      at Module._resolveFilename (node:internal/modules/cjs/loader:1075:15)
      at Module._load (node:internal/modules/cjs/loader:920:27)
      at Function.executeUserEntryPoint [as runMain] (node:internal/modules/run_main:81:12)
      at node:internal/main/run_main_module:23:47 {
    code: 'MODULE_NOT_FOUND',
    requireStack: []
  }
  
  Node.js v18.16.0
  [1]

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
  node:internal/modules/cjs/loader:1078
    throw err;
    ^
  
  Error: Cannot find module '$TESTCASE_ROOT/_build/default/output/main.js'
      at Module._resolveFilename (node:internal/modules/cjs/loader:1075:15)
      at Module._load (node:internal/modules/cjs/loader:920:27)
      at Function.executeUserEntryPoint [as runMain] (node:internal/modules/run_main:81:12)
      at node:internal/main/run_main_module:23:47 {
    code: 'MODULE_NOT_FOUND',
    requireStack: []
  }
  
  Node.js v18.16.0
  [1]
