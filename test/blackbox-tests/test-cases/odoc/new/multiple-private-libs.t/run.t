This test checks that there is no clash when two private libraries have the same name

  $ dune build --display short @doc-new 2>&1 | grep docs/test
          odoc _doc_new/html/docs/test@6aabb9861046/index.html
          odoc _doc_new/html/docs/test@ea8c79305c05/index.html
          odoc _doc_new/html/docs/test@6aabb9861046/Test
          odoc _doc_new/html/docs/test@ea8c79305c05/Test
