  $ $JBUILDER runtest -j1 --display short --root .
      ocamldep bar.ml.d
      ocamldep foo.ml.d
        ocamlc .foo.objs/foo.{cmi,cmo,cmt}
        ocamlc .bar.objs/bar.{cmi,cmo,cmt}
        ocamlc bar.cma
      ocamlopt .foo.objs/foo.{cmx,o}
      ocamlopt .bar.objs/bar.{cmx,o}
      ocamlopt bar.{a,cmxa}
      ocamlopt bar.cmxs
     ocamlfind test.exe (exit 2)
  (cd _build/default && /Users/rgrinberg/.opam/4.05.0/bin/ocamlfind ocamlc -linkpkg -package bar -o test.exe test.ml)
  ocamlfind: Package `foo' not found - required by `bar'
  [1]
