Compilation using WasmOO

  $ dune build bin/technologic.bc.wasm.js @install
  $ dune trace cat | jq -r 'include "dune";
  >   processes
  > | select(.args.prog | test("wasm_of_ocaml$"))
  > | .args | targets | .[] | sub("^_build/[^/]+/"; "")' | sort
  .js/default/js_of_ocaml-compiler.runtime/jsoo_runtime.wasma
  .js/default/js_of_ocaml/js_of_ocaml.wasma
  .js/default/stdlib/std_exit.wasmo
  .js/default/stdlib/stdlib.wasma
  bin/.technologic.eobjs/jsoo/dune__exe.wasmo
  bin/.technologic.eobjs/jsoo/dune__exe__Technologic.wasmo
  bin/.technologic.eobjs/jsoo/dune__exe__Z.wasmo
  bin/.technologic.eobjs/jsoo/runtime.bc.runtime.wasma
  bin/technologic.bc.wasm.assets
  bin/technologic.bc.wasm.js
  lib/.x.objs/jsoo/default/x.wasma
  $ node ./_build/default/bin/technologic.bc.wasm.js
  buy it
  use it
  break it
  fix it
  $ dune build bin/technologic.bc.wasm.js @install --profile release
  $ node ./_build/default/bin/technologic.bc.wasm.js
  buy it
  use it
  break it
  fix it
  $ cat >dune-workspace <<EOF
  > (lang dune 3.17)
  > (context
  >   (default (disable_dynamically_linked_foreign_archives true)))
  > EOF
  $ dune build bin/technologic.bc.wasm.js @install --profile dev
  $ dune build bin/technologic.bc.wasm.js @install --profile release
