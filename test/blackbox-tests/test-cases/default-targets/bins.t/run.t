Generates targets when modes is set for binaries:
  $ dune build --display short @all 2>&1 | grep '\.bc\|\.exe'
        ocamlc byteandnative.bc
        ocamlc byteandnative.bc-for-jsoo
        ocamlc bytecodeonly.bc
        ocamlc bytecodeonly.bc-for-jsoo
        ocamlc bytecodeonly.exe
      ocamlopt byteandnative.exe
      ocamlopt nativeonly.exe
