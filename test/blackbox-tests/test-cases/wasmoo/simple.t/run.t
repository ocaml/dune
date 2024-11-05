Compilation using WasmOO
  $ dune build bin/technologic.bc.wasm.js @install --profile dev
  $ node ./_build/default/bin/technologic.bc.wasm.js
  buy it
  use it
  break it
  fix it
  $ dune build @install --profile release
  $ node ./_build/default/bin/technologic.bc.wasm.js
  buy it
  use it
  break it
  fix it

Compilation using WasmOO with `disable_dynamically_linked_foreign_archives = true`

  $ cat >dune-workspace <<EOF
  > (lang dune 3.17)
  > (context
  >   (default (disable_dynamically_linked_foreign_archives true)))
  > EOF
  $ dune clean
  $ dune build bin/technologic.bc.wasm.js @install --profile dev

Wasm_of_ocaml whole program compilation works with
`disable_dynamically_linked_foreign_archives = true`:

  $ dune build bin/technologic.bc.wasm.js @install --profile release

We expect a runtime error when running this `bc-for-jsoo` file.

  $ ! if dune exe bin/technologic.bc-for-jsoo ; then true ; else false ; fi 2> /dev/null
