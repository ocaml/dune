  $ echo "(lang dune 1.0)" > dune-project
  $ mkdir -p lib

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (language c)
  >  (names add))
  > EOF

  $ dune build
  File "lib/dune", line 1, characters 0-44:
  1 | (foreign_library
  2 |  (language c)
  3 |  (names add))
  Error: 'foreign_library' is only available since version 2.0 of the dune
  language. Please update your dune-project file to have (lang 2.0).
  [1]

----------------------------------------------------------------------

  $ echo "(lang dune 2.0)" > dune-project

  $ dune build
  File "lib/dune", line 1, characters 0-44:
  1 | (foreign_library
  2 |  (language c)
  3 |  (names add))
  Error: field archive_name missing
  [1]

----------------------------------------------------------------------

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

----------------------------------------------------------------------

  $ cat >lib/mul.c <<EOF
  > #include <caml/mlvalues.h>
  > value mul(value x, value y) { return Val_int(Int_val(x) * Int_val(y)); }
  > EOF

  $ dune build

----------------------------------------------------------------------

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (archive_name addmul)
  >  (language c)
  >  (names add mul))
  > (library
  >  (name calc)
  >  (modules calc)
  >  (foreign_stubs_archives addmul config))
  > (executable
  >  (name main)
  >  (libraries calc)
  >  (modules main))
  > (foreign_library
  >  (archive_name config)
  >  (language c)
  >  (flags -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

  $ cat >lib/config.c <<EOF
  > #include <caml/mlvalues.h>
  > value config() { return Val_int(CONFIG_VALUE); }
  > EOF

  $ cat >lib/calc.ml <<EOF
  > external add : int -> int -> int = "add"
  > external mul : int -> int -> int = "mul"
  > external config : unit -> int = "config"
  > let calc x y z = add (mul (add x y) z) (config ())
  > EOF

  $ cat >lib/calc.mli <<EOF
  > val calc : int -> int -> int -> int
  > EOF

  $ cat >lib/main.ml <<EOF
  > let () = Printf.printf "%d" (Calc.calc 1 2 3)
  > EOF

  $ dune build

  $ dune exec lib/main.exe
  2009

  $ (cd _build/default && ocamlrun -I lib lib/main.bc)
  2009

----------------------------------------------------------------------

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (archive_name addmul)
  >  (language c)
  >  (names add mul))
  > (library
  >  (name calc)
  >  (modules calc)
  >  (foreign_stubs_archives addmul config))
  > (executable
  >  (name main)
  >  (libraries calc)
  >  (modules main))
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (include_dirs headers)
  >  (extra_deps eight.h)
  >  (flags -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

  $ dune build
  File "lib/dune", line 19, characters 8-14:
  19 |  (names config))
               ^^^^^^
  Error: Object "config" has no source; One of "config.cxx", "config.cc" or
  "config.cpp" must be present.
  [1]

----------------------------------------------------------------------

  $ cat >lib/config.cpp <<EOF
  > #include <caml/mlvalues.h>
  > #include "ten.h"
  > extern "C" value config() { return Val_int(CONFIG_VALUE + TEN); }
  > EOF

  $ mkdir -p lib/headers
  $ cat >lib/headers/ten.h <<EOF
  > #include "../eight.h"
  > #include "some/deep/path/one.h"
  > #define TEN (1 + EIGHT + ONE)
  > EOF

  $ mkdir -p lib/headers/some/deep/path
  $ cat >lib/headers/some/deep/path/one.h <<EOF
  > #define ONE 1
  > EOF

  $ cat >lib/eight.h <<EOF
  > #define EIGHT 8
  > EOF

  $ dune clean
  $ dune build --display short
        ocamlc lib/add$ext_obj
      ocamldep lib/.calc.objs/calc.mli.d
        ocamlc lib/.calc.objs/byte/calc.{cmi,cmti}
      ocamldep lib/.calc.objs/calc.ml.d
        ocamlc lib/.calc.objs/byte/calc.{cmo,cmt}
        ocamlc lib/calc.cma
        ocamlc lib/mul$ext_obj
    ocamlmklib lib/dlladdmul$ext_dll,lib/libaddmul$ext_lib
           gcc lib/config$ext_obj
    ocamlmklib lib/dllconfig$ext_dll,lib/libconfig$ext_lib
      ocamldep lib/.main.eobjs/main.ml.d
        ocamlc lib/.main.eobjs/byte/dune__exe__Main.{cmi,cmo,cmt}
      ocamlopt lib/.main.eobjs/native/dune__exe__Main.{cmx,o}
      ocamlopt lib/.calc.objs/native/calc.{cmx,o}
      ocamlopt lib/calc.{a,cmxa}
      ocamlopt lib/calc.cmxs
        ocamlc lib/main.bc
      ocamlopt lib/main.exe

  $ dune exec lib/main.exe
  2019

  $ (cd _build/default && ocamlrun -I lib lib/main.bc)
  2019

----------------------------------------------------------------------

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (archive_name addmul)
  >  (language c)
  >  (names add mul))
  > (library
  >  (name calc)
  >  (modules calc)
  >  (foreign_stubs_archives addmul config))
  > (executable
  >  (name main)
  >  (libraries calc)
  >  (modules main))
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (include_dirs headers another/dir)
  >  (extra_deps eight.h)
  >  (flags -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

  $ dune build
  File "lib/dune", line 16, characters 1-35:
  16 |  (include_dirs headers another/dir)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Include directory "another/dir" not found.
  [1]

----------------------------------------------------------------------

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (archive_name addmul)
  >  (language c)
  >  (names add mul))
  > (library
  >  (name calc)
  >  (modules calc)
  >  (foreign_stubs_archives addmul config))
  > (executable
  >  (name main)
  >  (libraries calc)
  >  (modules main))
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (include_dirs headers /absolute/path)
  >  (extra_deps eight.h)
  >  (flags -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

  $ dune build
  File "lib/dune", line 16, characters 1-38:
  16 |  (include_dirs headers /absolute/path)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: "/absolute/path" is an external directory; dependencies in external
  directories are currently not tracked.

----------------------------------------------------------------------

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (archive_name addmul)
  >  (language c)
  >  (names add))
  > (foreign_library
  >  (archive_name addmul)
  >  (language c)
  >  (names mul))
  > (library
  >  (name calc)
  >  (modules calc)
  >  (foreign_stubs_archives addmul config))
  > (executable
  >  (name main)
  >  (libraries calc)
  >  (modules main))
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (include_dirs headers /absolute/path)
  >  (extra_deps eight.h)
  >  (flags -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

  $ dune build
  File "lib/dune", line 5, characters 0-67:
  5 | (foreign_library
  6 |  (archive_name addmul)
  7 |  (language c)
  8 |  (names mul))
  Error: Multiple foreign libraries with the same archive name "addmul"; the
  name has already been taken in lib/dune:1.
  [1]
