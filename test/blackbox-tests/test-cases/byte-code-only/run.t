  $ env ORIG_PATH="$PATH" PATH="$PWD/ocaml-bin:$PATH" $JBUILDER build --root . -j1 --display short
  No rule found for src/.foo.objs/foo.cmx
      ocamldep bin/toto.ml.d
      ocamldep src/foo.ml.d
        ocamlc bin/.toto.eobjs/toto.{cmi,cmo,cmt}
        ocamlc src/.foo.objs/foo.{cmi,cmo,cmt}
        ocamlc bin/toto.exe
        ocamlc src/foo.cma
  [1]
