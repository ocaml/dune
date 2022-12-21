tests js_of_ocaml conigs

  $ dune build bin/bin1.bc.js bin/bin2.bc.js bin/bin3.bc.js --display short
   js_of_ocaml bin/.bin1.eobjs/jsoo/bin1.bc.runtime.js
   js_of_ocaml .js/stdlib/std_exit.cmo.js
   js_of_ocaml .js/stdlib/stdlib.cma.js
        ocamlc lib/.library1.objs/byte/library1.{cmi,cmo,cmt}
   js_of_ocaml bin/.bin2.eobjs/jsoo/bin2.bc.runtime.js
   js_of_ocaml bin/.bin3.eobjs/jsoo/bin3.bc.runtime.js
        ocamlc bin/.bin1.eobjs/byte/dune__exe__Bin1.{cmi,cmti}
        ocamlc bin/.bin2.eobjs/byte/dune__exe__Bin2.{cmi,cmti}
        ocamlc bin/.bin3.eobjs/byte/dune__exe__Bin3.{cmi,cmti}
        ocamlc lib/library1.cma
        ocamlc bin/.bin1.eobjs/byte/dune__exe__Bin1.{cmo,cmt}
        ocamlc bin/.bin2.eobjs/byte/dune__exe__Bin2.{cmo,cmt}
        ocamlc bin/.bin3.eobjs/byte/dune__exe__Bin3.{cmo,cmt}
   js_of_ocaml lib/.library1.objs/jsoo/library1.cma.js
   js_of_ocaml bin/.bin1.eobjs/jsoo/dune__exe__Bin1.cmo.js
   js_of_ocaml bin/.bin2.eobjs/jsoo/dune__exe__Bin2.cmo.js
   js_of_ocaml bin/.bin3.eobjs/jsoo/dune__exe__Bin3.cmo.js
   js_of_ocaml bin/bin2.bc.js
   js_of_ocaml bin/bin3.bc.js
  File "bin/dune", line 2, characters 8-12:
  2 |   (name bin1)
              ^^^^
   js_of_ocaml bin/bin1.bc.js (exit 1)
  /home/hugo/.opam/4.14.0/bin/js_of_ocaml: Error: Incompatible build info detected while linking.
   - .bin1.eobjs/jsoo/bin1.bc.runtime.js: use-js-string=false
   - .bin1.eobjs/jsoo/dune__exe__Bin1.cmo.js: use-js-string=true
  [1]
  $ node _build/default/bin/bin1.bc.js
  internal/modules/cjs/loader.js:896
    throw err;
    ^
  
  Error: Cannot find module '$TESTCASE_ROOT/_build/default/bin/bin1.bc.js'
      at Function.Module._resolveFilename (internal/modules/cjs/loader.js:893:15)
      at Function.Module._load (internal/modules/cjs/loader.js:743:27)
      at Function.executeUserEntryPoint [as runMain] (internal/modules/run_main.js:72:12)
      at internal/main/run_main_module.js:17:47 {
    code: 'MODULE_NOT_FOUND',
    requireStack: []
  }
  [1]
  $ node _build/default/bin/bin2.bc.js
  Hello bin2
  Hi library1
  $ node _build/default/bin/bin3.bc.js
  Hello bin3
  Hi library1
