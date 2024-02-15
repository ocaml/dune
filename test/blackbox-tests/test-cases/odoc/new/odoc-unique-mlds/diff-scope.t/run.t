Duplicate mld's in different scope
  $ dune build @doc-new --display short 2>&1 | grep page
          odoc _doc_new/index/page-docs.odoc
          odoc _doc_new/index/stdlib/page-stdlib.odoc
          odoc _doc_new/index/local/page-local.odoc
          odoc _doc_new/index/local/scope2/page-scope2.odoc
          odoc _doc_new/index/local/scope1/page-scope1.odoc
          odoc _doc_new/odoc/local/scope2/page-foo.odoc
          odoc _doc_new/odoc/local/scope1/page-foo.odoc
          odoc _doc_new/index/stdlib/page-stdlib.odocl
          odoc _doc_new/index/local/page-local.odocl
          odoc _doc_new/index/page-docs.odocl
          odoc _doc_new/index/local/scope2/page-scope2.odocl
          odoc _doc_new/odoc/local/scope2/page-foo.odocl
          odoc _doc_new/index/local/scope1/page-scope1.odocl
          odoc _doc_new/odoc/local/scope1/page-foo.odocl
