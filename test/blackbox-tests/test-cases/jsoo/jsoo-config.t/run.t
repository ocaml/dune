tests js_of_ocaml conigs

  $ dune build bin/bin1.bc.js bin/bin2.bc.js bin/bin3.bc.js
  $ dune trace cat | jq_dune -r '
  >   processes
  > | select(.args.prog | test("js_of_ocaml$"))
  > | .args | targets | .[] | sub("^_build/[^/]+/"; "")' \
  > | sort \
  > | censor
  .js/effects=disabled+use-js-string=false@$DIGEST1/.runtime/$DIGEST2/runtime.bc.runtime.js
  .js/effects=disabled+use-js-string=false@$DIGEST1/stdlib/std_exit.cmo.js
  .js/effects=disabled+use-js-string=false@$DIGEST1/stdlib/stdlib.cma.js
  .js/effects=disabled@$DIGEST1/.runtime/$DIGEST2/runtime.bc.runtime.js
  .js/effects=disabled@$DIGEST1/stdlib/std_exit.cmo.js
  .js/effects=disabled@$DIGEST1/stdlib/stdlib.cma.js
  bin/.bin1.eobjs/jsoo/dune__exe__Bin1.cmo.js
  bin/.bin2.eobjs/jsoo/dune__exe__Bin2.cmo.js
  bin/.bin3.eobjs/jsoo/dune__exe__Bin3.cmo.js
  bin/bin1.bc.js
  bin/bin2.bc.js
  bin/bin3.bc.js
  lib/.library1.objs/jsoo/effects=disabled+use-js-string=false@$DIGEST1/library1.cma.js
  lib/.library1.objs/jsoo/effects=disabled@$DIGEST1/library1.cma.js
  $ node _build/default/bin/bin1.bc.js
  Hello bin1
  Hi library1
  $ node _build/default/bin/bin2.bc.js
  Hello bin2
  Hi library1
  $ node _build/default/bin/bin3.bc.js
  Hello bin3
  Hi library1
