Compilation of libraries with pulic-names

  $ dune build --display short
        ocamlc a/.a.objs/byte/a.{cmi,cmo,cmt}
   js_of_ocaml b/.main.eobjs/jsoo/main.bc.runtime.js
      ocamlopt a/.a.objs/native/a.{cmx,o}
        ocamlc b/.main.eobjs/byte/dune__exe__Main.{cmi,cmti}
        ocamlc a/a.cma
   js_of_ocaml .js/default/stdlib/std_exit.cmo.js
   js_of_ocaml .js/default/stdlib/stdlib.cma.js
      ocamlopt a/a.{a,cmxa}
        ocamlc b/.main.eobjs/byte/dune__exe__Main.{cmo,cmt}
   js_of_ocaml a/.a.objs/jsoo/default/a.cma.js
      ocamlopt a/a.cmxs
        ocamlc b/main.bc-for-jsoo
   js_of_ocaml b/.main.eobjs/jsoo/dune__exe__Main.cmo.js
   js_of_ocaml b/main.bc.js
