  $ echo "(lang dune 3.0)" > dune-project

----------------------------------------------------------------------------------
* Error message for a missing source file.

  $ mkdir lib

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (archive_name addmul)
  >  (language c)
  >  (names add mul))
  > EOF

  $ cat >lib/add.c <<EOF
  > #include <caml/mlvalues.h>
  > value add(value x, value y) { return Val_int(Int_val(x) + Int_val(y)); }
  > EOF

  $ dune build
  File "lib/dune", line 4, characters 12-15:
  4 |  (names add mul))
                  ^^^
  Error: Object "mul" has no source; "mul.c" must be present.
  [1]

----------------------------------------------------------------------------------
* Successful build of a foreign library archive when all source files exist.

  $ cat >lib/mul.c <<EOF
  > #include <caml/mlvalues.h>
  > value mul(value x, value y) { return Val_int(Int_val(x) * Int_val(y)); }
  > EOF

  $ dune build
----------------------------------------------------------------------------------
* Error message for a missing C++ source file.

  $ cat >lib/dune <<EOF
  > (library
  >  (name calc)
  >  (modules calc)
  >  (foreign_archives config))
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (names config))
  > EOF
  $ touch lib/calc.ml

  $ rm -rf _build
  $ dune build
  File "lib/dune", line 8, characters 8-14:
  8 |  (names config))
              ^^^^^^
  Error: Object "config" has no source; One of "config.cc", "config.cpp" or
  "config.cxx" must be present.
  [1]
