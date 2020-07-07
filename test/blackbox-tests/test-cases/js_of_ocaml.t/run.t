  $ dune build --display short bin/technologic.bc.js @install
        ocamlc lib/stubs$ext_obj
    ocamlmklib lib/dllx_stubs$ext_dll,lib/libx_stubs$ext_lib
      ocamlopt .ppx/3edf09989a28fce237f8b735bd39446a/ppx.exe
           ppx lib/y.pp.ml
      ocamldep lib/.x.objs/y.pp.ml.d
        ocamlc lib/.x.objs/byte/x__.{cmi,cmo,cmt}
        ocamlc lib/.x.objs/byte/x__Y.{cmi,cmo,cmt}
      ocamlopt lib/.x.objs/native/x__Y.{cmx,o}
   js_of_ocaml bin/technologic.bc.runtime.js
           ppx bin/technologic.pp.ml
      ocamldep bin/.technologic.eobjs/technologic.pp.ml.d
           ppx bin/z.pp.ml
      ocamldep bin/.technologic.eobjs/z.pp.ml.d
   js_of_ocaml .js/js_of_ocaml/js_of_ocaml.cma.js
           ppx lib/x.pp.ml
      ocamldep lib/.x.objs/x.pp.ml.d
        ocamlc lib/.x.objs/byte/x.{cmi,cmo,cmt}
        ocamlc lib/x.cma
   js_of_ocaml lib/.x.objs/x.cma.js
      ocamlopt lib/.x.objs/native/x__.{cmx,o}
   js_of_ocaml .js/stdlib/stdlib.cma.js
        ocamlc bin/.technologic.eobjs/byte/z.{cmi,cmo,cmt}
        ocamlc bin/.technologic.eobjs/byte/technologic.{cmi,cmo,cmt}
   js_of_ocaml bin/.technologic.eobjs/byte/technologic.cmo.js
      ocamlopt lib/.x.objs/native/x.{cmx,o}
      ocamlopt lib/x.{a,cmxa}
      ocamlopt lib/x.cmxs
   js_of_ocaml bin/.technologic.eobjs/byte/z.cmo.js
     jsoo_link bin/technologic.bc.js
  $ $NODE ./_build/default/bin/technologic.bc.js
  buy it
  use it
  break it
  fix it
  $ dune build --display short bin/technologic.bc.js @install --profile release
        ocamlc lib/.x.objs/byte/x__.{cmi,cmo,cmt}
        ocamlc lib/.x.objs/byte/x__Y.{cmi,cmo,cmt}
        ocamlc lib/.x.objs/byte/x.{cmi,cmo,cmt}
        ocamlc lib/x.cma
        ocamlc bin/.technologic.eobjs/byte/z.{cmi,cmo,cmt}
        ocamlc bin/.technologic.eobjs/byte/technologic.{cmi,cmo,cmt}
        ocamlc bin/technologic.bc
   js_of_ocaml bin/technologic.bc.js
      ocamlopt lib/.x.objs/native/x__.{cmx,o}
      ocamlopt lib/.x.objs/native/x__Y.{cmx,o}
      ocamlopt lib/.x.objs/native/x.{cmx,o}
      ocamlopt lib/x.{a,cmxa}
      ocamlopt lib/x.cmxs
  $ $NODE ./_build/default/bin/technologic.bc.js
  buy it
  use it
  break it
  fix it

Inline tests
------------

  $ dune runtest inline-tests
           run alias inline-tests/byte/runtest
  inline tests (Byte)
  inline tests (Byte)
           run alias inline-tests/native/runtest
  inline tests (Native)
  inline tests (Native)
          node alias inline-tests/js/runtest
  inline tests (JS)
  inline tests (JS)
