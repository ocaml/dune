  $ dune build --display short aa.exe bb.exe
  File "dune", line 3, characters 1-14:
  3 |  (c_names foo)
       ^^^^^^^^^^^^^
  Warning: 'c_names' was deprecated in version 2.0 of the dune language. Use
  the (foreign_stubs ...) stanza instead.
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
  $ touch err/foo.ml err/stubs.c
  $ cat > err/dune << EOF
  > (executable
  >  (name foo)
  >  (c_names stubs))
  > EOF
  $ dune build --root err @all
  Entering directory 'err'
  Info: Creating file dune-project with this contents:
  | (lang dune 2.0)
  File "dune", line 3, characters 1-16:
  3 |  (c_names stubs))
       ^^^^^^^^^^^^^^^
  Warning: 'c_names' was deprecated in version 2.0 of the dune language. Use
  the (foreign_stubs ...) stanza instead.
  File "dune", line 1, characters 0-41:
  1 | (executable
  2 |  (name foo)
  3 |  (c_names stubs))
  Error: Pure bytecode executables cannot contain foreign stubs.
  Did you forget to add `(modes exe)'?
  [1]
