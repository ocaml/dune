This test checks that there is no clash when two private libraries have the same name

  $ dune build --display short @doc-private
      ocamldep b/test.ml.d
        ocamlc b/.test.objs/test.{cmo,cmt,cmi}
          odoc _doc/_odoc/lib/test@b/test.odoc
      ocamldep a/test.ml.d
        ocamlc a/.test.objs/test.{cmo,cmt,cmi}
          odoc _doc/_odoc/lib/test@a/test.odoc
