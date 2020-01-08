  $ dune build --display short aa.exe bb.exe
        ocamlc .aa.eobjs/byte/dune__exe.{cmi,cmo,cmt}
      ocamlopt .aa.eobjs/native/dune__exe.{cmx,o}
      ocamldep .aa.eobjs/aa.ml.d
        ocamlc .aa.eobjs/byte/dune__exe__Aa.{cmi,cmo,cmt}
      ocamlopt .aa.eobjs/native/dune__exe__Aa.{cmx,o}
      ocamldep .aa.eobjs/bb.ml.d
        ocamlc .aa.eobjs/byte/dune__exe__Bb.{cmi,cmo,cmt}
      ocamlopt .aa.eobjs/native/dune__exe__Bb.{cmx,o}
        ocamlc foo$ext_obj
      ocamlopt bb.exe
      ocamlopt aa.exe

  $ _build/default/aa.exe
  A

  $ _build/default/bb.exe
  B

  $ mkdir err
  $ echo "(lang dune 2.1)" > err/dune-project
  $ touch err/foo.ml err/stubs.c
  $ cat > err/dune << EOF
  > (executable
  >  (name foo)
  >  (modes exe byte)
  >  (foreign_stubs (language c) (names stubs)))
  > EOF
  $ dune build --root err @all
  Entering directory 'err'
  File "dune", line 1, characters 0-86:
  1 | (executable
  2 |  (name foo)
  3 |  (modes exe byte)
  4 |  (foreign_stubs (language c) (names stubs)))
  Error: Pure bytecode executables cannot contain foreign stubs.
  Hint: If you only need to build a native executable use "(modes exe)".
  [1]
