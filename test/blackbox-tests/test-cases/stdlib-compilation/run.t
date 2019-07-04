Build stdlib with stdlib.ml
  $ dune build --display short @all --root with-lib-interface
  Entering directory 'with-lib-interface'
           cat src/camlinternalFormatBasics.pp.ml
      ocamldep src/.stdlib.objs/camlinternalFormatBasics.pp.ml.d
        ocamlc src/.stdlib.objs/byte/camlinternalFormatBasics.{cmi,cmo,cmt}
      ocamlopt src/.stdlib.objs/native/camlinternalFormatBasics.{cmx,o}
           cat src/list.pp.ml
      ocamldep src/.stdlib.objs/list.pp.ml.d
           cat src/std_exit.pp.ml
      ocamldep src/.stdlib.objs/std_exit.pp.ml.d
           cat src/stdlib.pp.ml
      ocamldep src/.stdlib.objs/stdlib.pp.ml.d
        ocamlc src/.stdlib.objs/byte/stdlib.{cmi,cmo,cmt}
        ocamlc src/.stdlib.objs/byte/stdlib__List.{cmi,cmo,cmt}
        ocamlc src/stdlib.cma
        ocamlc src/.stdlib.objs/byte/std_exit.{cmi,cmo,cmt}
      ocamlopt src/.stdlib.objs/native/std_exit.{cmx,o}
      ocamlopt src/.stdlib.objs/native/stdlib.{cmx,o}
      ocamlopt src/.stdlib.objs/native/stdlib__List.{cmx,o}
      ocamlopt src/stdlib.{a,cmxa}
      ocamlopt src/stdlib.cmxs

Build stdlib without stdlib.ml
  $ dune build --display short @all --root without-lib-interface
  Entering directory 'without-lib-interface'
           cat src/camlinternalFormatBasics.pp.ml
      ocamldep src/.stdlib.objs/camlinternalFormatBasics.pp.ml.d
        ocamlc src/.stdlib.objs/byte/camlinternalFormatBasics.{cmi,cmo,cmt}
      ocamlopt src/.stdlib.objs/native/camlinternalFormatBasics.{cmx,o}
           cat src/list.pp.ml
      ocamldep src/.stdlib.objs/list.pp.ml.d
        ocamlc src/.stdlib.objs/byte/stdlib__List.{cmi,cmo,cmt}
      ocamlopt src/.stdlib.objs/native/stdlib__List.{cmx,o}
           cat src/std_exit.pp.ml
      ocamldep src/.stdlib.objs/std_exit.pp.ml.d
      ocamlopt src/stdlib.{a,cmxa}
      ocamlopt src/stdlib.cmxs
        ocamlc src/stdlib.cma
        ocamlc src/.stdlib.objs/byte/std_exit.{cmi,cmo,cmt}
      ocamlopt src/.stdlib.objs/native/std_exit.{cmx,o}
