Duplicate mld's in the same scope
  $ jbuilder build @doc --display short --root ./same-scope 2>&1 | grep -v Entering
          odoc _doc/_html/odoc.css
        ocamlc lib1/.root_lib1.objs/root_lib1.{cmi,cmo,cmt}
          odoc _doc/_odoc/lib/root.lib1/root_lib1.odoc
        ocamlc lib2/.root_lib2.objs/root_lib2.{cmi,cmo,cmt}
          odoc _doc/_odoc/lib/root.lib2/root_lib2.odoc
          odoc _doc/_html/root/Root_lib1/.jbuilder-keep,_doc/_html/root/Root_lib1/index.html
          odoc _doc/_odoc/pkg/root/page-index.odoc
          odoc _doc/_html/root/index.html
          odoc _doc/_html/root/Root_lib2/.jbuilder-keep,_doc/_html/root/Root_lib2/index.html

Duplicate mld's in different scope
  $ rm -rf diff-scope/_build
  $ jbuilder build @doc --display short --root ./diff-scope 2>&1 | grep -v Entering
          odoc _doc/_html/odoc.css
        ocamlc scope1/.scope1.objs/scope1.{cmi,cmo,cmt}
          odoc _doc/_odoc/lib/scope1/scope1.odoc
          odoc _doc/_html/scope1/Scope1/.jbuilder-keep,_doc/_html/scope1/Scope1/index.html
          odoc _doc/_odoc/pkg/scope1/page-index.odoc
          odoc _doc/_html/scope1/index.html
        ocamlc scope2/.scope2.objs/scope2.{cmi,cmo,cmt}
          odoc _doc/_odoc/lib/scope2/scope2.odoc
          odoc _doc/_html/scope2/Scope2/.jbuilder-keep,_doc/_html/scope2/Scope2/index.html
          odoc _doc/_odoc/pkg/scope2/page-index.odoc
          odoc _doc/_html/scope2/index.html
