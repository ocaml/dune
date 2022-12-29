Using copy_files and alias to move asset from a library folder while keeping
the same layout as in the original source breaks the production of js rules
of the library modules

  $ dune build @melange
  $ node _build/default/app/output/app/x.js 2>&1 | grep "Cannot find"
  Error: Cannot find module '../lib/y.js'
