tests js_of_ocaml conigs

  $ dune build bin/bin1.bc.js bin/bin2.bc.js bin/bin3.bc.js
  $ dune trace cat | jq -r 'include "dune";
  >   processes
  > | select(.args.prog | test("js_of_ocaml$"))
  > | .args | targets | .[] | sub("^_build/[^/]+/"; "")' \
  > | sort \
  > | censor
  .js/!use-js-string/.runtime/$DIGEST/runtime.bc.runtime.js
  .js/!use-js-string/stdlib/std_exit.cmo.js
  .js/!use-js-string/stdlib/stdlib.cma.js
  .js/default/.runtime/$DIGEST/runtime.bc.runtime.js
  .js/default/stdlib/std_exit.cmo.js
  .js/default/stdlib/stdlib.cma.js
  .js/use-js-string/.runtime/$DIGEST/runtime.bc.runtime.js
  .js/use-js-string/stdlib/std_exit.cmo.js
  .js/use-js-string/stdlib/stdlib.cma.js
  bin/.bin1.eobjs/jsoo/dune__exe__Bin1.cmo.js
  bin/.bin2.eobjs/jsoo/dune__exe__Bin2.cmo.js
  bin/.bin3.eobjs/jsoo/dune__exe__Bin3.cmo.js
  bin/bin1.bc.js
  bin/bin2.bc.js
  bin/bin3.bc.js
  lib/.library1.objs/jsoo/!use-js-string/library1.cma.js
  lib/.library1.objs/jsoo/default/library1.cma.js
  lib/.library1.objs/jsoo/use-js-string/library1.cma.js
  $ node _build/default/bin/bin1.bc.js
  Hello bin1
  Hi library1
  $ node _build/default/bin/bin2.bc.js
  Hello bin2
  Hi library1
  $ node _build/default/bin/bin3.bc.js
  Hello bin3
  Hi library1
