This test checks that there is no clash when two private libraries have the same name

  $ $JBUILDER build -j1 --root . @doc
          odoc _doc/odoc.css
      ocamldep a/test.depends.ocamldep-output
      ocamldep a/test.dependsi.ocamldep-output
      ocamldep b/test.depends.ocamldep-output
      ocamldep b/test.dependsi.ocamldep-output
        ocamlc a/test.{cmi,cmo,cmt}
          odoc _doc/test@a/page-index.odoc
        ocamlc b/test.{cmi,cmo,cmt}
          odoc _doc/test@b/page-index.odoc
          odoc _doc/test@a/test.odoc
          odoc _doc/test@b/test.odoc
          odoc _doc/test@a/index.html
          odoc _doc/test@a/Test/.jbuilder-keep,_doc/test@a/Test/index.html
          odoc _doc/test@b/index.html
          odoc _doc/test@b/Test/.jbuilder-keep,_doc/test@b/Test/index.html
