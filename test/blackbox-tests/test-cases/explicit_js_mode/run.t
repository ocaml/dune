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
