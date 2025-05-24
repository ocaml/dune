This test checks that there is no clash when two private libraries have the same name

  $ dune build --display short @doc-new 2>&1 | grep docs/test
          odoc _doc_new/html/docs/test@38f98c954b37/Test
          odoc _doc_new/html/docs/test@38f98c954b37/index.html
