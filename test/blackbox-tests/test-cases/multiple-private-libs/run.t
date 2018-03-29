This test checks that there is no clash when two private libraries have the same name

  $ jbuilder build -j1 --display short --root . @doc-private
      ocamldep a/test.ml.d
        ocamlc a/.test.objs/test.{cmi,cmo,cmt}
          odoc _doc/_odoc/lib/test@a/test.odoc
      ocamldep b/test.ml.d
        ocamlc b/.test.objs/test.{cmi,cmo,cmt}
          odoc _doc/_odoc/lib/test@b/test.odoc
