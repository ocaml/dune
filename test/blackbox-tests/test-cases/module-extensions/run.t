Test the module_extensions new field inside the dune-project file.

The `dune build` should work.

  $ dune build --display short
      ocamldep .main.eobjs/main.mf.d
      ocamldep .main.eobjs/main.mfi.d
        ocamlc .main.eobjs/byte/main.{cmi,cmti}
      ocamlopt .main.eobjs/native/main.{cmx,o}
      ocamlopt main.exe
