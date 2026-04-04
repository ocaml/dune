----------------------------------------------------------------------------------
Tests for the (c_library_flags ...) field in foreign code stanzas.

  $ echo "(lang dune 3.23)" > dune-project

----------------------------------------------------------------------------------
* Build a library with (foreign_stubs ...) that uses (c_library_flags ...).

  $ mkdir stubs

  $ cat >stubs/dune <<EOF
  > (library
  >  (name stubs_lib)
  >  (modules stubs_lib)
  >  (foreign_stubs
  >   (language c)
  >   (names trig))
  >  (c_library_flags :standard -lm))
  > (executable
  >  (name main_stubs)
  >  (modes exe byte)
  >  (libraries stubs_lib)
  >  (modules main_stubs))
  > EOF

  $ cat >stubs/stubs_lib.ml <<EOF
  > external cos_int : int -> int = "cos_int"
  > EOF

  $ cat >stubs/stubs_lib.mli <<EOF
  > val cos_int : int -> int
  > EOF

  $ cat >stubs/main_stubs.ml <<EOF
  > let () = Printf.printf "%d\n" (Stubs_lib.cos_int 0)
  > EOF

  $ cat >stubs/trig.c <<EOF
  > #include <math.h>
  > #include <caml/mlvalues.h>
  > value cos_int(value x) {
  >   return Val_int((int) (cos((double) Int_val(x)) * 1000.0));
  > }
  > EOF

  $ dune exec ./stubs/main_stubs.exe
  1000

  $ dune build stubs/main_stubs.bc
  $ (cd _build/default && ocamlrun -I stubs stubs/main_stubs.bc)
  1000

* Build a foreign archive with (foreign_library ...) that uses (c_library_flags ...).

  $ mkdir foreign-library

  $ cat >foreign-library/dune <<EOF
  > (foreign_library
  >  (archive_name trig)
  >  (language c)
  >  (names trig)
  >  (c_library_flags :standard -lm))
  > (library
  >  (name foreign_library_lib)
  >  (modules foreign_library_lib)
  >  (foreign_archives trig))
  > (executable
  >  (name main_foreign_library)
  >  (modes exe byte)
  >  (libraries foreign_library_lib)
  >  (modules main_foreign_library))
  > EOF

  $ cat >foreign-library/foreign_library_lib.ml <<EOF
  > external cos_int : int -> int = "cos_int"
  > EOF

  $ cat >foreign-library/foreign_library_lib.mli <<EOF
  > val cos_int : int -> int
  > EOF

  $ cat >foreign-library/main_foreign_library.ml <<EOF
  > let () = Printf.printf "%d\n" (Foreign_library_lib.cos_int 0)
  > EOF

  $ cat >foreign-library/trig.c <<EOF
  > #include <math.h>
  > #include <caml/mlvalues.h>
  > value cos_int(value x) {
  >   return Val_int((int) (cos((double) Int_val(x)) * 1000.0));
  > }
  > EOF

  $ dune build foreign-library/main_foreign_library.bc
  $ (cd _build/default && ocamlrun -I foreign-library foreign-library/main_foreign_library.bc)
  1000
