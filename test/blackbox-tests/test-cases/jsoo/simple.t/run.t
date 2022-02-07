Compilation using jsoo

  $ dune build --display short bin/technologic.bc.js @install --profile dev 2>&1 | \
  > sed s,^\ *$(ocamlc -config-var c_compiler),\ \ C_COMPILER,g
   js_of_ocaml bin/technologic.bc.runtime.js
      ocamldep bin/.technologic.eobjs/technologic.ml.d
   js_of_ocaml .js/stdlib/std_exit.cmo.js
    C_COMPILER lib/stubs.o
      ocamldep lib/.x.objs/x.ml.d
        ocamlc lib/.x.objs/byte/x__.{cmi,cmo,cmt}
      ocamldep lib/.x.objs/y.ml.d
      ocamldep bin/.technologic.eobjs/z.ml.d
    ocamlmklib lib/dllx_stubs.so,lib/libx_stubs.a
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
  $ dune build --display short bin/technologic.bc.js @install --profile release 2>&1 | \
  > sed s,^\ *$(ocamlc -config-var c_compiler),\ \ C_COMPILER,g
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

Compilation using jsoo with disable_dynamically_linked_foreign_archives = true

  $ cat >dune-workspace <<EOF
  > (lang dune 2.0)
  > (context
  >   (default (disable_dynamically_linked_foreign_archives true)))
  > EOF
  $ dune clean
  $ dune build --display short bin/technologic.bc.js @install --profile dev 2>&1 | \
  > sed s,^\ *$(ocamlc -config-var c_compiler),\ \ C_COMPILER,g
   js_of_ocaml bin/technologic.bc.runtime.js
      ocamldep bin/.technologic.eobjs/technologic.ml.d
   js_of_ocaml .js/stdlib/std_exit.cmo.js
    C_COMPILER lib/stubs.o
      ocamldep lib/.x.objs/x.ml.d
        ocamlc lib/.x.objs/byte/x__.{cmi,cmo,cmt}
      ocamldep lib/.x.objs/y.ml.d
      ocamldep bin/.technologic.eobjs/z.ml.d
    ocamlmklib lib/libx_stubs.a
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

Js_of_ocaml whole program compilation doesn't work with
disable_dynamically_linked_foreign_archives = true
We would like the following to succeed:

  $ dune build --display short bin/technologic.bc.js @install --profile release 2>&1 | \
  > sed s,^\ *$(ocamlc -config-var c_compiler),\ \ C_COMPILER,g
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
      ocamlopt lib/x.cmxs
  File "bin/dune", line 2, characters 8-19:
  2 |  (names technologic)
              ^^^^^^^^^^^
  Error: No rule found for lib/dllx_stubs.so

Js_of_ocaml whole program compilation doesn't work with
disable_dynamically_linked_foreign_archives = true
We expect a runtime error when running this bc-for-jsoo file.

  $ dune exe bin/technologic.bc-for-jsoo
  File "bin/dune", line 2, characters 8-19:
  2 |  (names technologic)
              ^^^^^^^^^^^
  Error: No rule found for lib/dllx_stubs.so
  [1]
