Duplicate mld's in the same scope
  $ $JBUILDER build @doc -j1 --display short --root ./same-scope 2>&1 | grep -v Entering
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

Duplicate mld's in different scope
  $ rm -rf diff-scope/_build
  $ $JBUILDER build @doc -j1 --display short --root ./diff-scope 2>&1 | grep -v Entering
          odoc _doc/odoc.css
          odoc _doc/scope1/page-foo.odoc
          odoc _doc/scope1/page-index.odoc
        ocamlc scope1/.scope1.objs/scope1.{cmi,cmo,cmt}
          odoc _doc/scope2/page-foo.odoc
          odoc _doc/scope2/page-index.odoc
        ocamlc scope2/.scope2.objs/scope2.{cmi,cmo,cmt}
          odoc _doc/scope1/scope1.odoc
          odoc _doc/scope2/scope2.odoc
          odoc _doc/scope1/foo.html
          odoc _doc/scope1/index.html
          odoc _doc/scope1/Scope1/.jbuilder-keep,_doc/scope1/Scope1/index.html
          odoc _doc/scope2/foo.html
          odoc _doc/scope2/index.html
          odoc _doc/scope2/Scope2/.jbuilder-keep,_doc/scope2/Scope2/index.html
