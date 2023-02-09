This test checks that there is no clash when two private libraries have the same name

  $ dune build --display short @doc-private
          odoc _doc/_html/_odoc_support
        ocamlc a/.test.objs/byte/test.{cmi,cmo,cmt}
        ocamlc b/.test.objs/byte/test.{cmi,cmo,cmt}
          odoc a/.test.objs/byte/test.odoc
          odoc b/.test.objs/byte/test.odoc
          odoc _doc/_odocls/test@6aabb9861046/test.odocl
          odoc _doc/_odocls/test@ea8c79305c05/test.odocl
          odoc _doc/_html/test@6aabb9861046/Test/.dummy,_doc/_html/test@6aabb9861046/Test/index.html
          odoc _doc/_html/test@ea8c79305c05/Test/.dummy,_doc/_html/test@ea8c79305c05/Test/index.html
