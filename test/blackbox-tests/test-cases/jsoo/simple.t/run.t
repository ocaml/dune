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

Js_of_ocaml whole program compilation works with
disable_dynamically_linked_foreign_archives = true:

  $ dune build bin/technologic.bc.js @install --profile release

We expect a runtime error when running this bc-for-jsoo file.

  $ ! if dune exe bin/technologic.bc-for-jsoo ; then true ; else false ; fi 2> /dev/null
