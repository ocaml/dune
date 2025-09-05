Test linktime includes for an external library with C stubs

  $ mkdir lib1
  $ echo "(lang dune 1.11)" > lib1/dune-project
  $ echo "# DUNE_GEN" > lib1/META.dunetestlib1.template
  $ touch lib1/dunetestlib1.opam
  $ cat >lib1/lib1.ml <<EOF
  > external id : int -> int = "dune_test_id"
  > EOF
  $ cat >lib1/lib_stubs.c <<EOF
  > #include <caml/mlvalues.h>
  > CAMLprim value dune_test_id(value v) { return v; }
  > EOF
  $ cat >lib1/dune <<EOF
  > (library
  >  (public_name dunetestlib1)
  >  (name lib1)
  >  (modules lib1)
  >  (c_names lib_stubs))
  > EOF
  $ dune build --root lib1 @install
  Entering directory 'lib1'
  Leaving directory 'lib1'

First we create an external library and implementation
  $ mkdir exe
  $ echo "(lang dune 2.0)" > exe/dune-project
  $ cat >exe/dune <<EOF
  > (executable (name bar) (libraries dunetestlib1))
  > EOF
  $ cat >exe/bar.ml <<EOF
  > let () = Printf.printf "lib1: %d\n" (Lib1.id 42)
  > EOF

Then we make sure that it works fine.
  $ env OCAMLPATH=lib1/_build/install/default/lib: dune exec --root exe ./bar.exe
  Entering directory 'exe'
  Leaving directory 'exe'
  lib1: 42

