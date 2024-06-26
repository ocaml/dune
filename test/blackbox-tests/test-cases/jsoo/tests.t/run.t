tests stanza with jsoo

  $ dune build @default @runtest-js
  a: ok
  File "dune", line 2, characters 11-12:
  2 |   (names a b)
                 ^
  node:internal/modules/cjs/loader:1146
    throw err;
    ^
  
  Error: Cannot find module '$TESTCASE_ROOT/_build/default/b.bc.js'
      at Module._resolveFilename (node:internal/modules/cjs/loader:1143:15)
      at Module._load (node:internal/modules/cjs/loader:984:27)
      at Function.executeUserEntryPoint [as runMain] (node:internal/modules/run_main:142:12)
      at node:internal/main/run_main_module:28:49 {
    code: 'MODULE_NOT_FOUND',
    requireStack: []
  }
  
  Node.js v22.0.0-v8-canary20231204cf8ac0f493
  [1]
