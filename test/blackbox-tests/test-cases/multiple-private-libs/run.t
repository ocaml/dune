This test checks that there is no clash when two private libraries have the same name

  $ $JBUILDER build -j1 --root . @doc
          odoc _doc/odoc.css
      ocamldep a/test.depends.ocamldep-output
      ocamldep a/test.dependsi.ocamldep-output
      ocamldep b/test.depends.ocamldep-output
      ocamldep b/test.dependsi.ocamldep-output
        ocamlc a/.test.objs/test.{cmi,cmo,cmt}
        ocamlc b/.test.objs/test.{cmi,cmo,cmt}
          odoc a/.test.objs/test.odoc
          odoc b/.test.objs/test.odoc
          odoc _doc/test@a/index.html
          odoc _doc/test@a/Test/.jbuilder-keep,_doc/test@a/Test/index.html
          odoc _doc/test@b/index.html
          odoc _doc/test@b/Test/.jbuilder-keep,_doc/test@b/Test/index.html
