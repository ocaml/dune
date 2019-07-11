Test the (dialect ...) stanza inside the dune-project file.

  $ dune build --display short @install @fmt
          dune .formatted/dune
   ocamlformat .formatted/main.mfi
   ocamlformat .formatted/main.mf
      ocamldep .main.eobjs/main.mf.d
      ocamldep .main.eobjs/main.mfi.d
        ocamlc .main.eobjs/byte/main.{cmi,cmti}
      ocamlopt .main.eobjs/native/main.{cmx,o}
      ocamlopt main.exe
