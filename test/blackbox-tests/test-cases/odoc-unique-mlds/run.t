  $ $JBUILDER build @doc -j1 --display short --root .
          odoc _doc/odoc.css
          odoc _doc/root.lib1/page-index.odoc
          odoc _doc/root.lib1/page-test.odoc
        ocamlc lib1/.root_lib1.objs/root_lib1.{cmi,cmo,cmt}
          odoc _doc/root.lib2/page-index.odoc
          odoc _doc/root.lib2/page-test.odoc
        ocamlc lib2/.root_lib2.objs/root_lib2.{cmi,cmo,cmt}
          odoc _doc/root.lib1/root_lib1.odoc
          odoc _doc/root.lib2/root_lib2.odoc
          odoc _doc/root.lib1/index.html
          odoc _doc/root.lib1/test.html
          odoc _doc/root.lib1/Root_lib1/.jbuilder-keep,_doc/root.lib1/Root_lib1/index.html
          odoc _doc/root.lib2/index.html
          odoc _doc/root.lib2/test.html
          odoc _doc/root.lib2/Root_lib2/.jbuilder-keep,_doc/root.lib2/Root_lib2/index.html
