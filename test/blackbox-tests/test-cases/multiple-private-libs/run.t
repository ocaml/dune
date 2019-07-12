This test checks that there is no clash when two private libraries have the same name

  $ dune build --display short @doc-private
          odoc _doc/_html/highlight.pack.js,_doc/_html/odoc.css
      ocamldep b/.test.objs/test.ml.d
        ocamlc b/.test.objs/byte/test.{cmi,cmo,cmt}
          odoc b/.test.objs/byte/test.odoc
          odoc _doc/_html/test@4f4e394aec41/Test/.dune-keep,_doc/_html/test@4f4e394aec41/Test/index.html
      ocamldep a/.test.objs/test.ml.d
        ocamlc a/.test.objs/byte/test.{cmi,cmo,cmt}
          odoc a/.test.objs/byte/test.odoc
          odoc _doc/_html/test@574d7a9dbf61/Test/.dune-keep,_doc/_html/test@574d7a9dbf61/Test/index.html
