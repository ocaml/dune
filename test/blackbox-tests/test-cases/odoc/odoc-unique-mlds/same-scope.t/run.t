Duplicate mld's in the same scope
  $ dune build @doc --display short
          odoc _doc/_html/highlight.pack.js,_doc/_html/odoc.css
          odoc _doc/_odoc/pkg/root/page-index.odoc
        ocamlc lib1/.root_lib1.objs/byte/root_lib1.{cmi,cmo,cmt}
        ocamlc lib2/.root_lib2.objs/byte/root_lib2.{cmi,cmo,cmt}
          odoc lib1/.root_lib1.objs/byte/root_lib1.odoc
          odoc lib2/.root_lib2.objs/byte/root_lib2.odoc
          odoc _doc/_odocls/root/root_lib1.odocl
          odoc _doc/_odocls/root/root_lib2.odocl
          odoc _doc/_odocls/root/page-index.odocl
          odoc _doc/_html/root/Root_lib1/.dummy,_doc/_html/root/Root_lib1/index.html
          odoc _doc/_html/root/Root_lib2/.dummy,_doc/_html/root/Root_lib2/index.html
          odoc _doc/_html/root/index.html
