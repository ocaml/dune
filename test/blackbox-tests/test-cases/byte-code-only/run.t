  $ env ORIG_PATH="$PATH" PATH="$PWD/ocaml-bin:$PATH" dune build --display short
      ocamldep bin/.toto.eobjs/toto.ml.d
        ocamlc bin/.toto.eobjs/byte/toto.{cmi,cmo,cmt}
        ocamlc bin/toto.exe
      ocamldep src/.foo.objs/foo.ml.d
        ocamlc src/.foo.objs/byte/foo.{cmi,cmo,cmt}
        ocamlc src/foo.cma

Check that building a native only executable fails
  $ env ORIG_PATH="$PATH" PATH="$PWD/ocaml-bin:$PATH" dune build --display short native-only/foo.exe
  Don't know how to build native-only/foo.exe
  [1]
