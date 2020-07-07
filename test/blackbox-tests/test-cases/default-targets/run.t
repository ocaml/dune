Generates targets when modes is set for binaries:
  $ dune build --root bins --display short @all 2>&1 | grep '\.bc\|\.exe'
        ocamlc byteandnative.bc
        ocamlc bytecodeonly.bc
        ocamlc bytecodeonly.exe
      ocamlopt byteandnative.exe
      ocamlopt nativeonly.exe

Generate targets when modes are set for libraries

  $ dune build --root libs --display short @all 2>&1 | grep 'cma\|cmxa\|cmxs'
        ocamlc byteandnative.cma
        ocamlc byteonly.cma
      ocamlopt byteandnative.{a,cmxa}
      ocamlopt nativeonly.{a,cmxa}
      ocamlopt byteandnative.cmxs
      ocamlopt nativeonly.cmxs
