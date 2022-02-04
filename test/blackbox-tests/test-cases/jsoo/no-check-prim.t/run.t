Compilation using jsoo

  $ dune build --display short bin/technologic.bc.js @install  2>&1 | \
  > sed s,^\ *$(ocamlc -config-var c_compiler),\ \ C_COMPILER,g
   js_of_ocaml bin/technologic.bc.runtime.js
      ocamldep bin/.technologic.eobjs/technologic.ml.d
   js_of_ocaml .js/stdlib/std_exit.cmo.js
      ocamldep lib/.x.objs/x.ml.d
        ocamlc lib/.x.objs/byte/x__.{cmi,cmo,cmt}
      ocamldep lib/.x.objs/y.ml.d
      ocamldep bin/.technologic.eobjs/z.ml.d
      ocamlopt lib/.x.objs/native/x__.{cmx,o}
        ocamlc lib/.x.objs/byte/x__Y.{cmi,cmo,cmt}
   js_of_ocaml .js/js_of_ocaml-compiler.runtime/jsoo_runtime.cma.js
   js_of_ocaml .js/js_of_ocaml/js_of_ocaml.cma.js
   js_of_ocaml .js/stdlib/stdlib.cma.js
      ocamlopt lib/.x.objs/native/x__Y.{cmx,o}
        ocamlc lib/.x.objs/byte/x.{cmi,cmo,cmt}
      ocamlopt lib/.x.objs/native/x.{cmx,o}
        ocamlc bin/.technologic.eobjs/byte/z.{cmi,cmo,cmt}
        ocamlc lib/x.cma
      ocamlopt lib/x.{a,cmxa}
        ocamlc bin/.technologic.eobjs/byte/technologic.{cmi,cmo,cmt}
   js_of_ocaml bin/.technologic.eobjs/byte/z.cmo.js
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
        ocamlc bin/.technologic.eobjs/byte/z.{cmi,cmo,cmt}
        ocamlc lib/x.cma
      ocamlopt lib/.x.objs/native/x.{cmx,o}
        ocamlc bin/.technologic.eobjs/byte/technologic.{cmi,cmo,cmt}
      ocamlopt lib/x.{a,cmxa}
        ocamlc bin/technologic.bc-for-jsoo
      ocamlopt lib/x.cmxs
   js_of_ocaml bin/technologic.bc.js
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
  $ dune build --display short bin/technologic.bc.js @install --profile dev
   js_of_ocaml bin/technologic.bc.runtime.js
        ocamlc lib/.x.objs/byte/x__.{cmi,cmo,cmt}
        ocamlc lib/.x.objs/byte/x__Y.{cmi,cmo,cmt}
      ocamlopt lib/.x.objs/native/x__.{cmx,o}
        ocamlc lib/.x.objs/byte/x.{cmi,cmo,cmt}
      ocamlopt lib/.x.objs/native/x__Y.{cmx,o}
        ocamlc bin/.technologic.eobjs/byte/z.{cmi,cmo,cmt}
      ocamlopt lib/.x.objs/native/x.{cmx,o}
        ocamlc lib/x.cma
        ocamlc bin/.technologic.eobjs/byte/technologic.{cmi,cmo,cmt}
      ocamlopt lib/x.{a,cmxa}
   js_of_ocaml bin/technologic.bc.js
      ocamlopt lib/x.cmxs
  $ dune build --display short bin/technologic.bc.js @install --profile release
        ocamlc lib/.x.objs/byte/x__.{cmi,cmo,cmt}
        ocamlc lib/.x.objs/byte/x__Y.{cmi,cmo,cmt}
      ocamlopt lib/.x.objs/native/x__.{cmx,o}
        ocamlc lib/.x.objs/byte/x.{cmi,cmo,cmt}
      ocamlopt lib/.x.objs/native/x__Y.{cmx,o}
        ocamlc bin/.technologic.eobjs/byte/z.{cmi,cmo,cmt}
        ocamlc lib/x.cma
      ocamlopt lib/.x.objs/native/x.{cmx,o}
        ocamlc bin/.technologic.eobjs/byte/technologic.{cmi,cmo,cmt}
      ocamlopt lib/x.{a,cmxa}
   js_of_ocaml bin/technologic.bc.js
      ocamlopt lib/x.cmxs
