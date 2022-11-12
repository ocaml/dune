However, it is possible to build a private one explicitly.

  $ dune build --display=short myprivatelib.so 2>&1 | dune_cmd sanitize
      ocamldep .myprivatelib.eobjs/myprivatelib.ml.d
        ocamlc .myprivatelib.eobjs/byte/myprivatelib.{cmi,cmo,cmt}
      ocamlopt .myprivatelib.eobjs/native/myprivatelib.{cmx,o}
      ocamlopt myprivatelib$ext_dll
