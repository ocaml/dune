When a public executable is built in shared_object mode, a specific error
message is displayed:

  $ dune build --root=public --display=short
  Entering directory 'public'
  File "dune", line 1, characters 0-70:
  1 | (executable
  2 |  (name mylib)
  3 |  (public_name mylib)
  4 |  (modes shared_object))
  Error: No installable mode found for this executable.
  One of the following modes is required:
  - exe
  - native
  - byte
  [1]

However, it is possible to build a private one explicitly.

  $ dune build --root=private --display=short myprivatelib.so
  Entering directory 'private'
      ocamldep .myprivatelib.eobjs/myprivatelib.ml.d
        ocamlc .myprivatelib.eobjs/byte/myprivatelib.{cmi,cmo,cmt}
      ocamlopt .myprivatelib.eobjs/native/myprivatelib.{cmx,o}
      ocamlopt myprivatelib$ext_dll
