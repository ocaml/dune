Check that .bc.js rule is generated only if js mode is used.

  $ dune build --display short a.bc.js
  Error: Don't know how to build a.bc.js
  Hint: did you mean b.bc.js?
  [1]

  $ dune build --display short b.bc.js
   js_of_ocaml b.bc.runtime.js
      ocamldep .b.eobjs/b.ml.d
        ocamlc .b.eobjs/byte/b.{cmi,cmo,cmt}
   js_of_ocaml .b.eobjs/byte/b.cmo.js
   js_of_ocaml .js/stdlib/stdlib.cma.js
     jsoo_link b.bc.js

Same for libraries.

  $ dune build --display short _build/default/.foo.objs/foo.cma.js
  Error: Don't know how to build _build/default/.foo.objs/foo.cma.js
  [1]

  $ dune build --display short _build/default/.bar.objs/bar.cma.js
        ocamlc .bar.objs/byte/bar.{cmi,cmo,cmt}
      ocamldep .bar.objs/d.ml.d
        ocamlc .bar.objs/byte/bar__D.{cmi,cmo,cmt}
        ocamlc bar.cma
   js_of_ocaml .bar.objs/bar.cma.js

Check that js targets are attached to @all

  $ dune clean
  $ dune build --display short @all
      ocamldep $ext_lib.eobjs/a.ml.d
        ocamlc $ext_lib.eobjs/byte/a.{cmi,cmo,cmt}
        ocamlc a.bc
      ocamldep .b.eobjs/b.ml.d
        ocamlc .b.eobjs/byte/b.{cmi,cmo,cmt}
        ocamlc b.bc
   js_of_ocaml b.bc.runtime.js
   js_of_ocaml .b.eobjs/byte/b.cmo.js
        ocamlc .bar.objs/byte/bar.{cmi,cmo,cmt}
      ocamldep .bar.objs/d.ml.d
        ocamlc .bar.objs/byte/bar__D.{cmi,cmo,cmt}
        ocamlc bar.cma
        ocamlc .foo.objs/byte/foo.{cmi,cmo,cmt}
      ocamlopt .foo.objs/native/foo.{cmx,o}
      ocamldep .foo.objs/c.ml.d
        ocamlc .foo.objs/byte/foo__C.{cmi,cmo,cmt}
        ocamlc foo.cma
      ocamlopt $ext_lib.eobjs/native/a.{cmx,o}
      ocamlopt a.exe
   js_of_ocaml .js/stdlib/stdlib.cma.js
     jsoo_link b.bc.js
      ocamlopt .foo.objs/native/foo__C.{cmx,o}
      ocamlopt foo.{a,cmxa}
      ocamlopt foo.cmxs
