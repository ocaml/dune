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
* Error message for a missing C++ source file.

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (archive_name addmul)
  >  (language c)
  >  (names add mul))
  > (library
  >  (name calc)
  >  (modules calc)
  >  (foreign_archives addmul config))
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (flags -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

  $ rm -rf _build
  $ dune build
  File "lib/dune", line 13, characters 8-14:
  13 |  (names config))
               ^^^^^^
  Error: Object "config" has no source; One of "config.cxx", "config.cc" or
  "config.cpp" must be present.
  [1]

----------------------------------------------------------------------------------
* Multiple (foreign_library ...) declarations.
* Mixing C and C++ foreign library archives.
* Passing flags via (flags ...) field.
* Interaction with (foreign_archives ...) stanza.

  $ cat >lib/config.cpp <<EOF
  > #include <caml/mlvalues.h>
  > extern "C" value config() { return Val_int(CONFIG_VALUE); }
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

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries calc)
  >  (modules main))
  > EOF

  $ cat >main.ml <<EOF
  > let () = Printf.printf "%d" (Calc.calc 1 2 3)
  > EOF

  $ rm -rf _build
  $ dune build

  $ dune exec ./main.exe
  2009

  $ (cd _build/default && ocamlrun -I lib main.bc)
  2009

----------------------------------------------------------------------------------
* Include directories via the (include_dirs ...) field.
* Extra dependencies via the (extra_deps ...) field.

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (archive_name addmul)
  >  (language c)
  >  (names add mul))
  > (library
  >  (name calc)
  >  (modules calc)
  >  (foreign_archives addmul config))
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (include_dirs headers)
  >  (extra_deps eight.h)
  >  (flags -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

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

  $ rm -rf _build
  $ dune build --display short
      ocamldep lib/.calc.objs/calc.mli.d
        ocamlc lib/.calc.objs/byte/calc.{cmi,cmti}
      ocamldep .main.eobjs/main.ml.d
        ocamlc .main.eobjs/byte/dune__exe__Main.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/native/dune__exe__Main.{cmx,o}
      ocamldep lib/.calc.objs/calc.ml.d
      ocamlopt lib/.calc.objs/native/calc.{cmx,o}
      ocamlopt lib/calc.{a,cmxa}
        ocamlc lib/add$ext_obj
        ocamlc lib/mul$ext_obj
    ocamlmklib lib/dlladdmul$ext_dll,lib/libaddmul$ext_lib
           gcc lib/config$ext_obj
    ocamlmklib lib/dllconfig$ext_dll,lib/libconfig$ext_lib
      ocamlopt main.exe
        ocamlc lib/.calc.objs/byte/calc.{cmo,cmt}
        ocamlc lib/calc.cma
        ocamlc main.bc
      ocamlopt lib/calc.cmxs

  $ dune exec ./main.exe
  2019

  $ (cd _build/default && ocamlrun -I lib main.bc)
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
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (include_dirs headers another/dir)
  >  (extra_deps eight.h)
  >  (flags -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

  $ dune build
  File "lib/dune", line 12, characters 23-34:
  12 |  (include_dirs headers another/dir)
                              ^^^^^^^^^^^
  Error: Include directory "another/dir" does not exist.
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
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (include_dirs headers /absolute/path)
  >  (extra_deps eight.h)
  >  (flags -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

  $ dune build
  File "lib/dune", line 12, characters 23-37:
  12 |  (include_dirs headers /absolute/path)
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
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (include_dirs headers /absolute/path)
  >  (extra_deps eight.h)
  >  (flags -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

  $ dune build
  File "lib/dune", line 6, characters 1-22:
  6 |  (archive_name addmul)
       ^^^^^^^^^^^^^^^^^^^^^
  Error: Multiple foreign libraries with the same archive name "addmul"; the
  name has already been taken in lib/dune:2.
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

  $ cat >main.ml <<EOF
  > let () = Printf.printf "%s %d" (Calc.month ()) (Calc.calc 1 2 3)
  > EOF

  $ rm -rf _build
  $ dune build

  $ dune exec ./main.exe
  October 2019

  $ (cd _build/default && ocamlrun -I lib main.bc)
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

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries calc)
  >  (foreign_archives lib/day)
  >  (modules main))
  > EOF

  $ cat >lib/day.c <<EOF
  > #include <caml/mlvalues.h>
  > value day() { return Val_int(8); }
  > EOF

  $ cat >main.ml <<EOF
  > external day : unit -> int = "day"
  > let () = Printf.printf "%d %s %d" (day ()) (Calc.month ()) (Calc.calc 1 2 3)
  > EOF

  $ dune build
  File "dune", line 1, characters 0-87:
  1 | (executable
  2 |  (name main)
  3 |  (libraries calc)
  4 |  (foreign_archives lib/day)
  5 |  (modules main))
  Error: Pure bytecode executables cannot contain foreign archives.
  Hint: If you need to build only a native executable use "(modes exe)".
  [1]

----------------------------------------------------------------------------------
* Interaction of (foreign_archives ...) and (executables ...).
* Foreign archives in subdirectories.

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

  $ cat >dune <<EOF
  > (executable
  >  (modes exe)
  >  (name main)
  >  (libraries calc)
  >  (foreign_archives lib/day)
  >  (modules main))
  > EOF

  $ cat >lib/day.c <<EOF
  > #include <caml/mlvalues.h>
  > value day() { return Val_int(8); }
  > EOF

  $ cat >main.ml <<EOF
  > external day : unit -> int = "day"
  > let () = Printf.printf "%d %s %d" (day ()) (Calc.month ()) (Calc.calc 1 2 3)
  > EOF

  $ rm -rf _build
  $ dune build

  $ dune exec ./main.exe
  8 October 2019

----------------------------------------------------------------------------------
* Use (env ...) to pass C++ flags.

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
  > (foreign_library
  >  (archive_name day)
  >  (language c)
  >  (names day))
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (include_dirs headers)
  >  (extra_deps eight.h)
  >  (names config))
  > EOF

  $ cat >dune <<EOF
  > (env (_ (cxx_flags -DCONFIG_VALUE=2000)))
  > (executable
  >  (modes exe)
  >  (name main)
  >  (libraries calc)
  >  (foreign_archives lib/day)
  >  (modules main))
  > EOF

  $ dune exec ./main.exe
  8 October 2019

----------------------------------------------------------------------------------
* Generated header.

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
  > (foreign_library
  >  (archive_name day)
  >  (language c)
  >  (names day))
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (include_dirs headers)
  >  (flags -DCONFIG_VALUE=2000)
  >  (extra_deps eight.h)
  >  (names config))
  > EOF

  $ mkdir -p lib2/headers
  $ cat >lib2/dune <<EOF
  > (foreign_library
  >  (archive_name today)
  >  (language c)
  >  (include_dirs headers)
  >  (names today))
  > EOF

  $ cat >lib2/headers/dune <<EOF
  > (rule
  >  (action (write-file today.h "#define TODAY \"Today\"")))
  > EOF

  $ cat >lib2/today.c <<EOF
  > #include <caml/mlvalues.h>
  > #include <caml/alloc.h>
  > #include "today.h"
  > value today() { return copy_string(TODAY); }
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (modes exe)
  >  (libraries calc)
  >  (foreign_archives lib/day lib2/today)
  >  (modules main))
  > EOF

  $ cat >main.ml <<EOF
  > external day : unit -> int = "day"
  > external today : unit -> string = "today"
  > let () = Printf.printf "%s: %d %s %d" (today ()) (day ()) (Calc.month ()) (Calc.calc 1 2 3)
  > EOF

  $ rm -rf _build
  $ dune exec --display short ./main.exe
      ocamldep lib/.calc.objs/calc.mli.d
        ocamlc lib/.calc.objs/byte/calc.{cmi,cmti}
      ocamldep .main.eobjs/main.ml.d
        ocamlc .main.eobjs/byte/dune__exe__Main.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/native/dune__exe__Main.{cmx,o}
      ocamldep lib/.calc.objs/calc.ml.d
      ocamlopt lib/.calc.objs/native/calc.{cmx,o}
      ocamlopt lib/calc.{a,cmxa}
        ocamlc lib/add$ext_obj
        ocamlc lib/mul$ext_obj
    ocamlmklib lib/dlladdmul$ext_dll,lib/libaddmul$ext_lib
        ocamlc lib/month$ext_obj
    ocamlmklib lib/dllcalc_stubs$ext_dll,lib/libcalc_stubs$ext_lib
           gcc lib/config$ext_obj
    ocamlmklib lib/dllconfig$ext_dll,lib/libconfig$ext_lib
        ocamlc lib/day$ext_obj
    ocamlmklib lib/dllday$ext_dll,lib/libday$ext_lib
        ocamlc lib2/today$ext_obj
    ocamlmklib lib2/dlltoday$ext_dll,lib2/libtoday$ext_lib
      ocamlopt main.exe
  Today: 8 October 2019

----------------------------------------------------------------------------------
* Object files with the same name in different archives.
* Generated C source file.

  $ mkdir -p lib3
  $ cat >lib3/dune <<EOF
  > (foreign_library
  >  (archive_name new_day)
  >  (language c)
  >  (names day))
  > (rule
  >  (action (write-file day.c "#include <caml/mlvalues.h>\nvalue new_day() { return Val_int(14); }\n")))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (modes exe)
  >  (libraries calc)
  >  (foreign_archives lib/day lib2/today lib3/new_day)
  >  (modules main))
  > EOF

  $ cat >main.ml <<EOF
  > external day : unit -> int = "day"
  > external today : unit -> string = "today"
  > external new_day : unit -> int = "new_day"
  > let () = Printf.printf "%s: %02d %s %d\n" (today ()) (    day ()) (Calc.month ()) (Calc.calc 1 2 3);
  >          Printf.printf "%s: %02d %s %d\n" (today ()) (new_day ()) (Calc.month ()) (Calc.calc 1 2 3);
  > EOF

  $ rm -rf _build
  $ dune exec ./main.exe
  Today: 08 October 2019
  Today: 14 October 2019

  $ dune rules _build/default/*/day.*
  ((deps
    ((File (In_source_tree lib/day.c))
     (Sandbox_config ((disallow symlink) (disallow copy)))))
   (targets ((In_build_dir _build/default/lib/day.c)))
   (action (copy lib/day.c _build/default/lib/day.c)))
  
  ((deps
    ((File (External /usr/local/home/amokhov/code/.opam/default/bin/ocamlc.opt))
     (File (In_build_dir _build/default/lib/day.c))
     (File (In_build_dir _build/default/lib/eight.h))
     (Sandbox_config ((disallow symlink) (disallow copy)))))
   (targets ((In_build_dir _build/default/lib/day$ext_obj)))
   (context default)
   (action
    (chdir
     _build/default/lib
     (run
      /usr/local/home/amokhov/code/.opam/default/bin/ocamlc.opt
      -g
      -ccopt
      -std=gnu99
      -ccopt
      -O2
      -ccopt
      -fno-strict-aliasing
      -ccopt
      -fwrapv
      -ccopt
      -fno-builtin-memcmp
      -ccopt
      -fPIC
      -ccopt
      -g
      -o
      day$ext_obj
      day.c))))
  
  ((deps ((Sandbox_config ())))
   (targets ((In_build_dir _build/default/lib3/day.c)))
   (context default)
   (action
    (chdir
     _build/default/lib3
     (write-file
      day.c
      "#include <caml/mlvalues.h>\nvalue new_day() { return Val_int(14); }\n"))))
  
  ((deps
    ((File (External /usr/local/home/amokhov/code/.opam/default/bin/ocamlc.opt))
     (File (In_build_dir _build/default/lib3/day.c))
     (Sandbox_config ((disallow symlink) (disallow copy)))))
   (targets ((In_build_dir _build/default/lib3/day$ext_obj)))
   (context default)
   (action
    (chdir
     _build/default/lib3
     (run
      /usr/local/home/amokhov/code/.opam/default/bin/ocamlc.opt
      -g
      -ccopt
      -std=gnu99
      -ccopt
      -O2
      -ccopt
      -fno-strict-aliasing
      -ccopt
      -fwrapv
      -ccopt
      -fno-builtin-memcmp
      -ccopt
      -fPIC
      -ccopt
      -g
      -o
      day$ext_obj
      day.c))))
