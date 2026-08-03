Compilation using jsoo

  $ js=bin/technologic.bc.js
  $ built_js=./_build/default/$js
  $ dune build "$js"
  $ dune trace cat | jq_dune -r '
  >   processes
  > | select(.args.prog | test("js_of_ocaml$"))
  > | .args | targets | .[] | sub("^_build/[^/]+/"; "")' | sort
  .js/effects=disabled/js_of_ocaml-compiler.runtime/jsoo_runtime.cma.js
  .js/effects=disabled/js_of_ocaml/js_of_ocaml.cma.js
  .js/effects=disabled/stdlib/std_exit.cmo.js
  .js/effects=disabled/stdlib/stdlib.cma.js
  bin/.technologic.eobjs/jsoo/runtime.bc.runtime.js
  bin/.technologic.eobjs/jsoo/technologic.cmo.js
  bin/.technologic.eobjs/jsoo/z.cmo.js
  bin/technologic.bc.js
  lib/.x.objs/jsoo/effects=disabled/x.cma.js
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
  $ cat >dune-workspace <<EOF
  > (lang dune 2.0)
  > (context
  >   (default (disable_dynamically_linked_foreign_archives true)))
  > EOF
  $ dune build "$js" --profile dev
  $ dune build "$js" --profile release
