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
  Entering directory 'exe'
      ocamlopt bar.exe (exit 2)
  (cd _build/default && /Users/rgrinberg/.opam/4.09.0/bin/ocamlopt.opt -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g -o bar.exe $TESTCASE_ROOT/lib1/_build/install/default/lib/dunetestlib1/lib1.cmxa .bar.eobjs/native/dune__exe__Bar.cmx)
  ld: library not found for -llib1_stubs
  clang: error: linker command failed with exit code 1 (use -v to see invocation)
  File "caml_startup", line 1:
  Error: Error during linking
  [1]

