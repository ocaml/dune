tests js_of_ocaml conigs

  $ dune build bin/bin1.bc.js bin/bin2.bc.js bin/bin3.bc.js --display short
   js_of_ocaml bin/.bin1.eobjs/jsoo/bin1.bc.runtime.js
   js_of_ocaml bin/.bin2.eobjs/jsoo/bin2.bc.runtime.js
   js_of_ocaml bin/.bin3.eobjs/jsoo/bin3.bc.runtime.js
   js_of_ocaml .js/use-js-string/stdlib/std_exit.cmo.js
   js_of_ocaml .js/use-js-string/stdlib/stdlib.cma.js
        ocamlc lib/.library1.objs/byte/library1.{cmi,cmo,cmt}
   js_of_ocaml .js/!use-js-string/stdlib/std_exit.cmo.js
   js_of_ocaml .js/!use-js-string/stdlib/stdlib.cma.js
   js_of_ocaml .js/default/stdlib/std_exit.cmo.js
   js_of_ocaml .js/default/stdlib/stdlib.cma.js
        ocamlc bin/.bin1.eobjs/byte/dune__exe__Bin1.{cmi,cmti}
   js_of_ocaml lib/.library1.objs/jsoo/use-js-string/library1.cmo.js
        ocamlc bin/.bin2.eobjs/byte/dune__exe__Bin2.{cmi,cmti}
   js_of_ocaml lib/.library1.objs/jsoo/!use-js-string/library1.cmo.js
   js_of_ocaml lib/.library1.objs/jsoo/default/library1.cmo.js
        ocamlc bin/.bin3.eobjs/byte/dune__exe__Bin3.{cmi,cmti}
        ocamlc bin/.bin1.eobjs/byte/dune__exe__Bin1.{cmo,cmt}
   js_of_ocaml lib/.library1.objs/jsoo/use-js-string/library1.cma.js
        ocamlc bin/.bin2.eobjs/byte/dune__exe__Bin2.{cmo,cmt}
   js_of_ocaml lib/.library1.objs/jsoo/!use-js-string/library1.cma.js
   js_of_ocaml lib/.library1.objs/jsoo/default/library1.cma.js
        ocamlc bin/.bin3.eobjs/byte/dune__exe__Bin3.{cmo,cmt}
   js_of_ocaml bin/.bin1.eobjs/jsoo/use-js-string/dune__exe__Bin1.cmo.js
   js_of_ocaml bin/.bin2.eobjs/jsoo/!use-js-string/dune__exe__Bin2.cmo.js
   js_of_ocaml bin/.bin3.eobjs/jsoo/default/dune__exe__Bin3.cmo.js
   js_of_ocaml bin/bin1.bc.js
   js_of_ocaml bin/bin2.bc.js
   js_of_ocaml bin/bin3.bc.js
  $ node _build/default/bin/bin1.bc.js
  Hello bin1
  Hi library1
  $ node _build/default/bin/bin2.bc.js
  Hello bin2
  Hi library1
  $ node _build/default/bin/bin3.bc.js
  Hello bin3
  Hi library1
