This test checks that there is no clash when two private libraries have the same name

  $ $JBUILDER build -j1 --display short --root . @doc
          odoc _doc/odoc.css
          odoc _doc/test@a/page-index.odoc
      ocamldep a/test.ml.d
          odoc _doc/test@b/page-index.odoc
      ocamldep b/test.ml.d
        ocamlc a/test.{cmi,cmo,cmt}
        ocamlc b/test.{cmi,cmo,cmt}
          odoc _doc/test@a/test.odoc
          odoc _doc/test@b/test.odoc
          odoc _doc/test@a/index.html
          odoc _doc/test@a/Test/.jbuilder-keep,_doc/test@a/Test/index.html
          odoc _doc/test@b/index.html
          odoc _doc/test@b/Test/.jbuilder-keep,_doc/test@b/Test/index.html
