----------------------------------------------------------------------------------
Testsuite for the (foreign_library ...) stanza.

----------------------------------------------------------------------------------
* (foreign_library ...) is unavailable before Dune 2.0.

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

----------------------------------------------------------------------------------
* (foreign_library ...) is available in Dune 2.0.
* "archive_name" is a required field.

  $ echo "(lang dune 2.0)" > dune-project

  $ dune build
  File "lib/dune", line 1, characters 0-44:
  1 | (foreign_library
  2 |  (language c)
  3 |  (names add))
  Error: field archive_name missing
  [1]

----------------------------------------------------------------------------------
* Error message for a missing source file.

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

  $ dune build --display short
        ocamlc lib/add$ext_obj
        ocamlc lib/mul$ext_obj
    ocamlmklib lib/dlladdmul$ext_dll,lib/libaddmul$ext_lib

----------------------------------------------------------------------------------
* Multiple (foreign_library ...) declarations.
* Passing flags via (flags ...) field.
* Interaction with (foreign_archives ...) stanza.

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (archive_name addmul)
  >  (language c)
  >  (names add mul))
  > (library
  >  (name calc)
  >  (modules calc)
  >  (foreign_archives addmul config))
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

----------------------------------------------------------------------------------
* Error message for a missing C++ source.

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (archive_name addmul)
  >  (language c)
  >  (names add mul))
  > (library
  >  (name calc)
  >  (modules calc)
  >  (foreign_archives addmul config))
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

----------------------------------------------------------------------------------
* Mixing C and C++ foreign library archives.
* Include directories via the (include_dirs ...) field.
* Extra dependencies via the (extra_deps ...) field.

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

----------------------------------------------------------------------------------
* Error message when a given (include_dir ...) is not found.

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (archive_name addmul)
  >  (language c)
  >  (names add mul))
  > (library
  >  (name calc)
  >  (modules calc)
  >  (foreign_archives addmul config))
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
  File "lib/dune", line 16, characters 23-34:
  16 |  (include_dirs headers another/dir)
                              ^^^^^^^^^^^
  Error: Include directory "another/dir" not found.
  [1]

----------------------------------------------------------------------------------
* Warning about untracked dependencies in external include directories.

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (archive_name addmul)
  >  (language c)
  >  (names add mul))
  > (library
  >  (name calc)
  >  (modules calc)
  >  (foreign_archives addmul config))
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
  File "lib/dune", line 16, characters 23-37:
  16 |  (include_dirs headers /absolute/path)
                              ^^^^^^^^^^^^^^
  Error: "/absolute/path" is an external directory; dependencies in external
  directories are currently not tracked.
  Hint: You can specify "/absolute/path" as an untracked include directory like this:
  
    (flags -I /absolute/path)
  
  [1]



----------------------------------------------------------------------------------
* Error message for multiple declarations with the same "archive_name".

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
  >  (foreign_archives addmul config))
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

----------------------------------------------------------------------------------
* Interaction of (foreign_stubs ...) and (foreign_archives ...).

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (archive_name addmul)
  >  (language c)
  >  (names add mul))
  > (library
  >  (name calc)
  >  (modules calc)
  >  (foreign_stubs (language c) (names month))
  >  (foreign_archives addmul config))
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

  $ cat >lib/month.c <<EOF
  > #include <caml/mlvalues.h>
  > #include <caml/alloc.h>
  > value month() { return copy_string("October"); }
  > EOF

  $ cat >lib/calc.ml <<EOF
  > external add : int -> int -> int = "add"
  > external mul : int -> int -> int = "mul"
  > external config : unit -> int = "config"
  > external month : unit -> string = "month"
  > let calc x y z = add (mul (add x y) z) (config ())
  > EOF

  $ cat >lib/calc.mli <<EOF
  > val calc : int -> int -> int -> int
  > val month : unit -> string
  > EOF

  $ cat >lib/main.ml <<EOF
  > let () = Printf.printf "%s %d" (Calc.month ()) (Calc.calc 1 2 3)
  > EOF

  $ dune build

  $ dune exec lib/main.exe
  October 2019

  $ (cd _build/default && ocamlrun -I lib lib/main.bc)
  October 2019

----------------------------------------------------------------------------------
* Error when using (foreign_archives ...) and a pure bytecode (executable ...).

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (archive_name addmul)
  >  (language c)
  >  (names add mul))
  > (library
  >  (name calc)
  >  (modules calc)
  >  (foreign_stubs (language c) (names month))
  >  (foreign_archives addmul config))
  > (executable
  >  (name main)
  >  (libraries calc)
  >  (foreign_archives day)
  >  (modules main))
  > (foreign_library
  >  (archive_name day)
  >  (language c)
  >  (names day))
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (include_dirs headers)
  >  (extra_deps eight.h)
  >  (flags -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

  $ cat >lib/day.c <<EOF
  > #include <caml/mlvalues.h>
  > value day() { return Val_int(8); }
  > EOF

  $ cat >lib/main.ml <<EOF
  > external day : unit -> int = "day"
  > let () = Printf.printf "%d %s %d" (day ()) (Calc.month ()) (Calc.calc 1 2 3)
  > EOF

  $ dune build
  File "lib/dune", line 10, characters 0-83:
  10 | (executable
  11 |  (name main)
  12 |  (libraries calc)
  13 |  (foreign_archives day)
  14 |  (modules main))
  Error: Pure bytecode executables cannot contain foreign archives.
  Hint: If you need to build only a native executable use "(modes exe)".
  [1]

----------------------------------------------------------------------------------
* Interaction of (foreign_archives ...) and (executables ...).

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (archive_name addmul)
  >  (language c)
  >  (names add mul))
  > (library
  >  (name calc)
  >  (modules calc)
  >  (foreign_stubs (language c) (names month))
  >  (foreign_archives addmul config))
  > (executable
  >  (name main)
  >  (modes exe)
  >  (libraries calc)
  >  (foreign_archives day)
  >  (modules main))
  > (foreign_library
  >  (archive_name day)
  >  (language c)
  >  (names day))
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (include_dirs headers)
  >  (extra_deps eight.h)
  >  (flags -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

  $ cat >lib/day.c <<EOF
  > #include <caml/mlvalues.h>
  > value day() { return Val_int(8); }
  > EOF

  $ cat >lib/main.ml <<EOF
  > external day : unit -> int = "day"
  > let () = Printf.printf "%d %s %d" (day ()) (Calc.month ()) (Calc.calc 1 2 3)
  > EOF

  $ dune build

  $ dune exec lib/main.exe
  8 October 2019
