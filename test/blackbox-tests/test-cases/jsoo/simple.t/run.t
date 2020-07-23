Compilation using jsoo

  $ dune build --display short bin/technologic.bc.js @install  2>&1 | \
  > sed s,^\ *$(ocamlc -config-var c_compiler),\ \ C_COMPILER,g
    C_COMPILER lib/stubs.o
      ocamlopt .ppx/7b799aed44581cc79b02033532c5f775/ppx.exe
        ocamlc lib/.x.objs/byte/x__.{cmi,cmo,cmt}
   js_of_ocaml .js/stdlib/std_exit.cmo.js
   js_of_ocaml bin/technologic.bc.runtime.js
    ocamlmklib lib/dllx_stubs.so,lib/libx_stubs.a
           ppx lib/x.pp.ml
           ppx lib/y.pp.ml
           ppx bin/technologic.pp.ml
           ppx bin/z.pp.ml
      ocamlopt lib/.x.objs/native/x__.{cmx,o}
      ocamldep lib/.x.objs/x.pp.ml.d
      ocamldep lib/.x.objs/y.pp.ml.d
      ocamldep bin/.technologic.eobjs/technologic.pp.ml.d
      ocamldep bin/.technologic.eobjs/z.pp.ml.d
        ocamlc lib/.x.objs/byte/x__Y.{cmi,cmo,cmt}
   js_of_ocaml .js/js_of_ocaml/js_of_ocaml.cma.js
   js_of_ocaml .js/stdlib/stdlib.cma.js
      ocamlopt lib/.x.objs/native/x__Y.{cmx,o}
        ocamlc lib/.x.objs/byte/x.{cmi,cmo,cmt}
      ocamlopt lib/.x.objs/native/x.{cmx,o}
        ocamlc bin/.technologic.eobjs/byte/z.{cmi,cmo,cmt}
        ocamlc lib/x.cma
      ocamlopt lib/x.{a,cmxa}
   js_of_ocaml bin/.technologic.eobjs/byte/z.cmo.js
        ocamlc bin/.technologic.eobjs/byte/technologic.{cmi,cmo,cmt}
   js_of_ocaml lib/.x.objs/x.cma.js
      ocamlopt lib/x.cmxs
   js_of_ocaml bin/.technologic.eobjs/byte/technologic.cmo.js
   js_of_ocaml bin/technologic.bc.js
  $ node ./_build/default/bin/technologic.bc.js
  buy it
  use it
  break it
  fix it
  $ dune build --display short bin/technologic.bc.js @install --profile release
        ocamlc lib/.x.objs/byte/x__.{cmi,cmo,cmt}
        ocamlc lib/.x.objs/byte/x__Y.{cmi,cmo,cmt}
      ocamlopt lib/.x.objs/native/x__.{cmx,o}
        ocamlc lib/.x.objs/byte/x.{cmi,cmo,cmt}
      ocamlopt lib/.x.objs/native/x__Y.{cmx,o}
        ocamlc lib/x.cma
      ocamlopt lib/.x.objs/native/x.{cmx,o}
        ocamlc bin/.technologic.eobjs/byte/z.{cmi,cmo,cmt}
      ocamlopt lib/x.{a,cmxa}
        ocamlc bin/.technologic.eobjs/byte/technologic.{cmi,cmo,cmt}
      ocamlopt lib/x.cmxs
        ocamlc bin/technologic.bc
   js_of_ocaml bin/technologic.bc.js
  $ node ./_build/default/bin/technologic.bc.js
  buy it
  use it
  break it
  fix it
