Using copy_files and alias to move asset from a library folder while keeping
the same layout as in the original source breaks the production of js rules
of the library modules

  $ dune build @melange
  Error: No rule found for app/output/lib/lib.js
  -> required by alias app/melange
  Error: No rule found for app/output/lib/y.js
  -> required by alias app/melange
  [1]
  $ node _build/default/app/output/app/x.js 2>&1 | grep "Cannot find"
  Error: Cannot find module '../lib/y.js'
