Compilation of libraries with pulic-names

  $ dune build --display short
      ocamldep a/.a.objs/a.impl.d
      ocamldep b/.main.eobjs/dune__exe__Main.impl.d
   js_of_ocaml b/.main.eobjs/jsoo/main.bc.runtime.js
        ocamlc a/.a.objs/byte/a.{cmi,cmo,cmt}
      ocamldep b/.main.eobjs/dune__exe__Main.intf.d
   js_of_ocaml .js/default/stdlib/std_exit.cmo.js
   js_of_ocaml .js/default/stdlib/stdlib.cma.js
      ocamlopt a/.a.objs/native/a.{cmx,o}
        ocamlc a/a.cma
        ocamlc b/.main.eobjs/byte/dune__exe__Main.{cmi,cmti}
      ocamlopt a/a.{a,cmxa}
   js_of_ocaml a/.a.objs/jsoo/default/a.cma.js
        ocamlc b/.main.eobjs/byte/dune__exe__Main.{cmo,cmt}
      ocamlopt a/a.cmxs
        ocamlc b/main.bc-for-jsoo
   js_of_ocaml b/.main.eobjs/jsoo/dune__exe__Main.cmo.js
   js_of_ocaml b/main.bc.js
