Compilation using jsoo
  $ js=bin/technologic.bc.js
  $ built_js=./_build/default/$js
  $ dune build "$js" --profile dev
  $ node "$built_js"
  buy it
  use it
  break it
  fix it
  $ dune build "$js" --profile release
  $ node "$built_js"
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
  $ dune build "$js" --profile dev

Js_of_ocaml whole program compilation works with
disable_dynamically_linked_foreign_archives = true:

  $ dune build "$js" --profile release

We expect a runtime error when running this bc-for-jsoo file.

  $ ! if dune exe bin/technologic.bc-for-jsoo ; then true ; else false ; fi 2> /dev/null
