----------------------------------------------------------------------------------
Testsuite for the (foreign_library ...) stanza.

  $ cat >sdune <<'EOF'
  > #!/usr/bin/env bash
  > DUNE_SANDBOX=symlink dune "$@"
  > EOF
  $ chmod +x sdune

----------------------------------------------------------------------------------
* (foreign_library ...) is unavailable before Dune 2.0.

  $ echo "(lang dune 1.0)" > dune-project
  $ mkdir -p lib

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (language c)
  >  (names add))
  > EOF

  $ ./sdune build
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

  $ ./sdune build
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

  $ ./sdune build
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

  $ ./sdune build
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
  $ ./sdune build
  File "lib/dune", line 13, characters 8-14:
  13 |  (names config))
               ^^^^^^
  Error: Object "config" has no source; One of "config.cc", "config.cpp" or
  "config.cxx" must be present.
  [1]

----------------------------------------------------------------------------------
* Multiple (foreign_library ...) declarations.
* Mixing C and C++ foreign library archives.
* Passing flags via (flags ...) field.
* Interaction with (foreign_archives ...) stanza.

  $ cat >lib/config.cpp <<EOF
  > #include <caml/mlvalues.h>
  > extern "C" value config(value unit) { return Val_int(CONFIG_VALUE); }
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
  $ ./sdune build

  $ ./sdune exec ./main.exe
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
  > extern "C" value config(value unit) { return Val_int(CONFIG_VALUE + TEN); }
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
  $ ./sdune build

  $ ./sdune exec ./main.exe
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

  $ ./sdune build
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

  $ ./sdune build
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

  $ ./sdune build
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
  > value month(value unit) { return caml_copy_string("October"); }
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
  $ ./sdune build

  $ ./sdune exec ./main.exe
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
  > value day(value unit) { return Val_int(8); }
  > EOF

  $ cat >main.ml <<EOF
  > external day : unit -> int = "day"
  > let () = Printf.printf "%d %s %d" (day ()) (Calc.month ()) (Calc.calc 1 2 3)
  > EOF

  $ ./sdune build
  File "dune", line 1, characters 0-87:
  1 | (executable
  2 |  (name main)
  3 |  (libraries calc)
  4 |  (foreign_archives lib/day)
  5 |  (modules main))
  Error: Pure bytecode executables cannot contain foreign archives.
  Hint: If you only need to build a native executable use "(modes exe)".
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
  > value day(value unit) { return Val_int(8); }
  > EOF

  $ cat >main.ml <<EOF
  > external day : unit -> int = "day"
  > let () = Printf.printf "%d %s %d" (day ()) (Calc.month ()) (Calc.calc 1 2 3)
  > EOF

  $ rm -rf _build
  $ ./sdune build

  $ ./sdune exec ./main.exe
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

  $ ./sdune exec ./main.exe
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
  > value today(value unit) { return caml_copy_string(TODAY); }
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
  $ ./sdune exec ./main.exe
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
  >  (action (write-file day.c "#include <caml/mlvalues.h>\nvalue new_day(value unit) { return Val_int(14); }\n")))
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
  $ ./sdune exec ./main.exe
  Today: 08 October 2019
  Today: 14 October 2019

----------------------------------------------------------------------------------
* Library directories in (include_dir ...)
* Build fails due to the missing "header.h"

  $ mkdir -p expansions/answer
  $ cat >expansions/dune <<EOF
  > (foreign_library
  >  (archive_name clib)
  >  (language c)
  >  (names src))
  > (executable
  >  (modes exe)
  >  (name main)
  >  (foreign_archives clib)
  >  (modules main))
  > EOF

  $ cat >expansions/answer/dune <<EOF
  > (library
  >  (name answer))
  > EOF

  $ cat >expansions/answer/header.h <<EOF
  > #define ANSWER 42
  > EOF

  $ cat >expansions/src.c <<EOF
  > #include <caml/mlvalues.h>
  > #include "header.h"
  > value answer(value unit) { return Val_int(ANSWER); }
  > EOF

  $ cat >expansions/main.ml <<EOF
  > external answer : unit -> int = "answer"
  > let () = Printf.printf "Answer = %d\n" (answer ());
  > EOF

  $ rm -rf _build
  $ ./sdune exec expansions/main.exe 2> /dev/null
  [1]

----------------------------------------------------------------------------------
* Library directories in (include_dir ...)
* Build succeeds

  $ cat >expansions/dune <<EOF
  > (foreign_library
  >  (archive_name clib)
  >  (language c)
  >  (include_dirs (lib answer))
  >  (names src))
  > (executable
  >  (modes exe)
  >  (name main)
  >  (foreign_archives clib)
  >  (modules main))
  > EOF

  $ rm -rf _build
  $ ./sdune exec expansions/main.exe
  Answer = 42

----------------------------------------------------------------------------------
* External library directories in (include_dir ...)
* Build fails with "dependencies in external directories are currently not tracked"
* TODO: Fix this limitation, and make this test succeed

  $ cat >expansions/dune <<EOF
  > (foreign_library
  >  (archive_name clib)
  >  (language c)
  >  (include_dirs (lib answer) (lib base))
  >  (names src))
  > (executable
  >  (modes exe)
  >  (name main)
  >  (foreign_archives clib)
  >  (modules main))
  > EOF

  $ rm -rf _build
  $ ./sdune exec expansions/main.exe 2> /dev/null
  [1]

----------------------------------------------------------------------------------
* External library directories in (include_dir ...)
* Build fails when using an uknown library

  $ cat >expansions/dune <<EOF
  > (foreign_library
  >  (archive_name clib)
  >  (language c)
  >  (include_dirs (lib answer) (lib unknown_lib))
  >  (names src))
  > (executable
  >  (modes exe)
  >  (name main)
  >  (foreign_archives clib)
  >  (modules main))
  > EOF

  $ rm -rf _build
  $ ./sdune exec expansions/main.exe
  File "expansions/dune", line 4, characters 33-44:
  4 |  (include_dirs (lib answer) (lib unknown_lib))
                                       ^^^^^^^^^^^
  Error: Library "unknown_lib" not found.
  Hint: try: dune external-lib-deps --missing expansions/main.exe
  [1]
