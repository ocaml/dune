Generates targets when modes is set for binaries:
  $ dune build --root bins --display short @all 2>&1 | grep '\.bc\|\.exe'
      ocamlopt byteandnative.exe
        ocamlc bytecodeonly.exe
      ocamlopt nativeonly.exe
        ocamlc byteandnative.bc
        ocamlc bytecodeonly.bc

Generate targets when modes are set for libraries

  $ dune build --root libs --display short @all 2>&1 | grep 'cma\|cmxa\|cmxs'
        ocamlc byteandnative.cma
        ocamlc byteonly.cma
      ocamlopt nativeonly.{a,cmxa}
      ocamlopt nativeonly.cmxs
      ocamlopt byteandnative.{a,cmxa}
      ocamlopt byteandnative.cmxs
