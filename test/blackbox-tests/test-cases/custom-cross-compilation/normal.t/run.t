  $ dune build --display short file @install
      ocamldep .p.eobjs/p.ml.d
      ocamldep .p.eobjs/p.ml.d [cross]
        ocamlc .p.eobjs/byte/p.{cmi,cmo,cmt}
        ocamlc .p.eobjs/byte/p.{cmi,cmo,cmt} [cross]
      ocamlopt .p.eobjs/native/p.{cmx,o}
      ocamlopt .p.eobjs/native/p.{cmx,o} [cross]
      ocamlopt p.exe
      ocamlopt p.exe [cross]
             p file
             p file [cross]

  $ cat _build/cross/file
  137
