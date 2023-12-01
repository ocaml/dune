Duplicate mld's in different scope
  $ dune build @doc --display short
          odoc _doc/_html/odoc.support
          odoc _doc/_odoc/pkg/scope1/page-foo.odoc
          odoc _doc/_odoc/pkg/scope1/page-index.odoc
        ocamlc scope1/.scope1.objs/byte/scope1.{cmi,cmo,cmt}
          odoc _doc/_odoc/pkg/scope2/page-foo.odoc
          odoc _doc/_odoc/pkg/scope2/page-index.odoc
        ocamlc scope2/.scope2.objs/byte/scope2.{cmi,cmo,cmt}
          odoc scope1/.scope1.objs/byte/scope1.odoc
          odoc scope2/.scope2.objs/byte/scope2.odoc
          odoc _doc/_odocls/scope1/scope1.odocl
          odoc _doc/_odocls/scope1/page-foo.odocl
          odoc _doc/_odocls/scope1/page-index.odocl
          odoc _doc/_odocls/scope2/scope2.odocl
          odoc _doc/_odocls/scope2/page-foo.odocl
          odoc _doc/_odocls/scope2/page-index.odocl
          odoc _doc/_html/scope1/Scope1/index.html
          odoc _doc/_html/scope1/foo.html
          odoc _doc/_html/scope1/index.html
          odoc _doc/_html/scope2/Scope2/index.html
          odoc _doc/_html/scope2/foo.html
          odoc _doc/_html/scope2/index.html
