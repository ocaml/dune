Melange rules should work with the copying sandbox

  $ make_melange_sandbox_project assets/data.txt

  $ mkdir -p assets
  $ echo "asset ok" > assets/data.txt

  $ rm -rf _build
  $ dune build @mel --sandbox copy --display quiet

Build artifacts are available in the melange target directory

  $ ls _build/default/output
  assets
  lib.js
  main.js
  $ cat _build/default/output/assets/data.txt
  asset ok
