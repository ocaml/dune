This test checks that there is no clash when two private libraries have the same name

  $ dune build --display short @doc-private
          odoc _doc/_html/odoc.support
        ocamlc a/.test.objs/byte/test.{cmi,cmo,cmt}
     sherlodoc _doc/_html/sherlodoc.js
        ocamlc b/.test.objs/byte/test.{cmi,cmo,cmt}
          odoc a/.test.objs/byte/test.odoc
          odoc b/.test.objs/byte/test.odoc
          odoc _doc/_odocls/test@6aabb9861046/test.odocl
          odoc _doc/_odocls/test@ea8c79305c05/test.odocl
     sherlodoc _doc/_html/test@6aabb9861046/db.js
     sherlodoc _doc/_html/test@ea8c79305c05/db.js
          odoc _doc/_html/test@6aabb9861046/Test/index.html
          odoc _doc/_html/test@ea8c79305c05/Test/index.html
