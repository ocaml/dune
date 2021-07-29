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
  When public_name is set, one of the following modes is required:
  - exe
  - native
  - byte
  [1]

However, it is possible to build a private one explicitly.

  $ dune build --root=private --display=short myprivatelib.so 2>&1 | dune_cmd sanitize
  Entering directory 'private'
      ocamldep .myprivatelib.eobjs/myprivatelib.ml.d
        ocamlc .myprivatelib.eobjs/byte/myprivatelib.{cmi,cmo,cmt}
      ocamlopt .myprivatelib.eobjs/native/myprivatelib.{cmx,o}
      ocamlopt myprivatelib$ext_dll

Byte_complete is allowed to be installable since 3.6

  $ mkdir byte-complete && cd byte-complete
  $ pkg=foobarbaz
  $ bin=testbin
  $ cat >dune-project <<EOF
  > (lang dune 3.5)
  > EOF
  $ touch $pkg.opam testbin.ml
  $ cat >dune <<EOF
  > (executable
  >  (name $bin)
  >  (public_name $bin)
  >  (modes byte_complete))
  > EOF
  $ dune build @install
  File "dune", line 4, characters 8-21:
  4 |  (modes byte_complete))
              ^^^^^^^^^^^^^
  Error: byte_complete is only available since version 3.6 of the dune
  language. Please update your dune-project file to have (lang dune 3.6).
  [1]
  $ cat >dune-project <<EOF
  > (lang dune 3.6)
  > EOF
  $ dune build $pkg.install
  $ grep $bin _build/default/$pkg.install
    "_build/install/default/bin/testbin"
  $ cd ..
