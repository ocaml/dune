Compilation using jsoo

  $ dune build bin/technologic.bc.js @install
  $ dune trace cat | jq -r 'include "dune";
  >   processes
  > | select(.args.prog | test("js_of_ocaml$"))
  > | .args | targets | .[] | sub("^_build/[^/]+/"; "")' | sort
  .js/default/js_of_ocaml-compiler.runtime/jsoo_runtime.cma.js
  .js/default/js_of_ocaml/js_of_ocaml.cma.js
  .js/default/stdlib/std_exit.cmo.js
  .js/default/stdlib/stdlib.cma.js
  bin/.technologic.eobjs/jsoo/runtime.bc.runtime.js
  bin/.technologic.eobjs/jsoo/technologic.cmo.js
  bin/.technologic.eobjs/jsoo/z.cmo.js
  bin/technologic.bc.js
  lib/.x.objs/jsoo/default/x.cma.js
  $ node ./_build/default/bin/technologic.bc.js
  buy it
  use it
  break it
  fix it
  $ dune build bin/technologic.bc.js @install --profile release
  $ node ./_build/default/bin/technologic.bc.js
  buy it
  use it
  break it
  fix it
  $ cat >dune-workspace <<EOF
  > (lang dune 2.0)
  > (context
  >   (default (disable_dynamically_linked_foreign_archives true)))
  > EOF
  $ dune build bin/technologic.bc.js @install --profile dev
  $ dune build bin/technologic.bc.js @install --profile release
