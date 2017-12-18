  $ $JBUILDER runtest -j1 --root .
         refmt bar.re.ml
         refmt cppome.re.ml
      ocamldep pp/reasononlypp.depends.ocamldep-output
         refmt hello.re.ml
         refmt pped.re.ml
        ocamlc rlib.{cmi,cmo,cmt}
         refmt cppome.re.mli
         refmt foo.re.mli
         refmt hello.re.mli
         refmt pped.re.mli
        ocamlc pp/reasononlypp.{cmi,cmo,cmt}
      ocamlopt rlib.{cmx,o}
      ocamlopt pp/reasononlypp.{cmx,o}
      ocamlopt pp/reasononlypp.exe
  reasononlypp cppome.re.pp.ml (exit 1)
  (cd _build/default && ./pp/reasononlypp.exe cppome.re.ml) > _build/default/cppome.re.pp.ml
  cppome.re.ml is not a reason source
  [1]
