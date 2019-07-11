Test the (dialect ...) stanza inside the dune-project file.

  $ dune build --display short @install
      ocamldep .main.eobjs/main.mf.d
      ocamldep .main.eobjs/main.mfi.d
        ocamlc .main.eobjs/byte/main.{cmi,cmti}
      ocamlopt .main.eobjs/native/main.{cmx,o}
      ocamlopt main.exe
