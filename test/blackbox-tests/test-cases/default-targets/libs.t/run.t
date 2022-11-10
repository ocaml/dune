Generate targets when modes are set for libraries

  $ dune build --display short @all 2>&1 | grep 'cma\|cmxa\|cmxs'
        ocamlc byteandnative.cma
        ocamlc byteonly.cma
      ocamlopt byteandnative.{a,cmxa}
      ocamlopt nativeonly.{a,cmxa}
      ocamlopt byteandnative.cmxs
      ocamlopt nativeonly.cmxs
