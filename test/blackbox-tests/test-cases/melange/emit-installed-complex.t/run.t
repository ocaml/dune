Test that libs are correctly installed in a complex setup

Build js files
  $ dune build @dist
  $ node _build/default/dist/app.js
  node:internal/modules/cjs/loader:936
    throw err;
    ^

    Error: Cannot find module 'pkg2/npm/npm.js'
  Require stack:
  - $TESTCASE_ROOT/_build/default/dist/node_modules/pkg1/inner/lib1/lib1.js
  - $TESTCASE_ROOT/_build/default/dist/app.js
      at Function.Module._resolveFilename (node:internal/modules/cjs/loader:933:15)
      at Function.Module._load (node:internal/modules/cjs/loader:778:27)
      at Module.require (node:internal/modules/cjs/loader:999:19)
      at require (node:internal/modules/cjs/helpers:102:18)
      at Object.<anonymous> ($TESTCASE_ROOT/_build/default/dist/node_modules/pkg1/inner/lib1/lib1.js:4:11)
      at Module._compile (node:internal/modules/cjs/loader:1099:14)
      at Object.Module._extensions..js (node:internal/modules/cjs/loader:1153:10)
      at Module.load (node:internal/modules/cjs/loader:975:32)
      at Function.Module._load (node:internal/modules/cjs/loader:822:12)
      at Module.require (node:internal/modules/cjs/loader:999:19) {
    code: 'MODULE_NOT_FOUND',
    requireStack: [
      '$TESTCASE_ROOT/_build/default/dist/node_modules/pkg1/inner/lib1/lib1.js',
      '$TESTCASE_ROOT/_build/default/dist/app.js'
    ]
  }

    Node.js v17.8.0
  [1]
