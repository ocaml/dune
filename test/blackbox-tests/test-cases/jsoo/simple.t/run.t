Compilation using jsoo
  $ dune build bin/technologic.bc.js @install --profile dev
  $ node ./_build/default/bin/technologic.bc.js
  buy it
  use it
  break it
  fix it
  $ dune build @install --profile release
  $ node ./_build/default/bin/technologic.bc.js
  buy it
  use it
  break it
  fix it

Compilation using jsoo with disable_dynamically_linked_foreign_archives = true

  $ cat >dune-workspace <<EOF
  > (lang dune 2.0)
  > (context
  >   (default (disable_dynamically_linked_foreign_archives true)))
  > EOF
  $ dune clean
  $ dune build bin/technologic.bc.js @install --profile dev

Js_of_ocaml whole program compilation doesn't work with
disable_dynamically_linked_foreign_archives = true
We would like the following to succeed:

  $ dune build bin/technologic.bc.js @install --profile release
  File "bin/dune", line 2, characters 8-19:
  2 |  (names technologic)
              ^^^^^^^^^^^
  Error: No rule found for lib/dllx_stubs.so
  [1]

Js_of_ocaml whole program compilation doesn't work with
disable_dynamically_linked_foreign_archives = true
We expect a runtime error when running this bc-for-jsoo file.

  $ dune exe bin/technologic.bc-for-jsoo
  File "bin/dune", line 2, characters 8-19:
  2 |  (names technologic)
              ^^^^^^^^^^^
  Error: No rule found for lib/dllx_stubs.so
  [1]
