  $ dune runtest --display short
      ocamllex pp/pp.ml
      ocamldep pp/.pp.eobjs/pp.ml.d
        ocamlc pp/.pp.eobjs/byte/pp.{cmi,cmo,cmt}
      ocamlopt pp/.pp.eobjs/native/pp.{cmx,o}
      ocamlopt pp/pp.exe
            pp dune/test.pp.ml
      ocamldep dune/.test.eobjs/test.pp.ml.d
        ocamlc dune/.test.eobjs/byte/test.{cmi,cmo,cmt}
      ocamlopt dune/.test.eobjs/native/test.{cmx,o}
      ocamlopt dune/test.exe
          test dune/test.output
