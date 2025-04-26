----------------------------------------------------------------------------------
Testsuite for the (foreign_library ...) stanza.

  $ export DUNE_SANDBOX=symlink

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
  File "lib/dune", lines 1-3, characters 0-44:
  1 | (foreign_library
  2 |  (language c)
  3 |  (names add))
  Error: 'foreign_library' is only available since version 2.0 of the dune
  language. Please update your dune-project file to have (lang dune 2.0).
  [1]

----------------------------------------------------------------------------------
* (foreign_library ...) is available in Dune 2.0.
* "archive_name" is a required field.

  $ echo "(lang dune 2.0)" > dune-project

  $ dune build
  File "lib/dune", lines 1-3, characters 0-44:
  1 | (foreign_library
  2 |  (language c)
  3 |  (names add))
  Error: Field "archive_name" is missing
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

  $ dune build
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
  $ touch lib/calc.ml

  $ rm -rf _build
  $ dune build
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
  >  (modes exe byte)
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
  $ dune build

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
* Error when specifying a non-existing external directory in (include_dirs... )

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
  >  (include_dirs headers /some/path)
  >  (extra_deps eight.h)
  >  (flags -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

  $ dune build
  File "lib/dune", line 12, characters 23-33:
  12 |  (include_dirs headers /some/path)
                              ^^^^^^^^^^
  Error: Unable to read the include directory.
  Reason: stat(/some/path): No such file or directory.
  [1]

----------------------------------------------------------------------------------
* Error when specifying an external file in (include_dirs... )

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
  >  (include_dirs headers /usr/bin/env)
  >  (extra_deps eight.h)
  >  (flags -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

  $ dune build
  File "lib/dune", line 12, characters 23-35:
  12 |  (include_dirs headers /usr/bin/env)
                              ^^^^^^^^^^^^
  Error: Unable to read the include directory.
  Reason: "/usr/bin/env" is not a directory.
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
  >  (include_dirs headers)
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
  >  (modes exe byte)
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

  $ dune build
  File "dune", lines 1-6, characters 0-105:
  1 | (executable
  2 |  (name main)
  3 |  (modes exe byte)
  4 |  (libraries calc)
  5 |  (foreign_archives lib/day)
  6 |  (modules main))
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
  $ dune exec ./main.exe
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
  $ dune exec ./main.exe
  Today: 08 October 2019
  Today: 14 October 2019

----------------------------------------------------------------------------------
* Library directories in (include_dir ...)
* Build fails due to the missing "header.h"

  $ mkdir -p some/dir/answer
  $ cat >some/dir/dune <<EOF
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

  $ cat >some/dir/answer/dune <<EOF
  > (library
  >  (name answer))
  > EOF

  $ cat >some/dir/answer/header.h <<EOF
  > #define ANSWER 42
  > EOF

  $ cat >some/dir/src.c <<EOF
  > #include <caml/mlvalues.h>
  > #include "header.h"
  > value answer(value unit) { return Val_int(ANSWER); }
  > EOF

  $ cat >some/dir/main.ml <<EOF
  > external answer : unit -> int = "answer"
  > let () = Printf.printf "Answer = %d\n" (answer ());
  > EOF

  $ rm -rf _build
  $ dune exec some/dir/main.exe 2> /dev/null
  [1]

----------------------------------------------------------------------------------
* Library directories in (include_dir ...)
* Build succeeds

  $ cat >some/dir/dune <<EOF
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
  $ dune exec some/dir/main.exe
  Answer = 42

----------------------------------------------------------------------------------
* External library directories in (include_dir ...)
* Using an external directory in (include_dir ...)

  $ mkdir -p external

  $ cat >external/dune-project <<EOF
  > (lang dune 2.1)
  > (name external_library)
  > EOF

  $ cat >external/dune <<EOF
  > (library
  >  (name extlib)
  >  (public_name external_library)
  >  (install_c_headers correction))
  > EOF

  $ cat >external/correction.h <<EOF
  > #define CORRECTION (-21)
  > EOF

  $ rm -rf _build
  $ touch external/external_library.opam
  $ ( cd external && dune build @install \
  > && dune install --prefix install --display=short 2>&1 | dune_cmd sanitize )
  Installing install/lib/external_library/META
  Installing install/lib/external_library/correction.h
  Installing install/lib/external_library/dune-package
  Installing install/lib/external_library/extlib$ext_lib
  Installing install/lib/external_library/extlib.cma
  Installing install/lib/external_library/extlib.cmi
  Installing install/lib/external_library/extlib.cmt
  Installing install/lib/external_library/extlib.cmx
  Installing install/lib/external_library/extlib.cmxa
  Installing install/lib/external_library/extlib.ml
  Installing install/lib/external_library/opam
  Installing install/lib/external_library/extlib.cmxs

  $ echo "(lang dune 2.1)" > some/dir/dune-project
  $ cat >some/dir/dune <<EOF
  > (foreign_library
  >  (archive_name clib)
  >  (language c)
  >  (include_dirs (lib answer) (lib external_library))
  >  (names src))
  > (executable
  >  (modes exe)
  >  (name main)
  >  (foreign_archives clib)
  >  (modules main))
  > EOF

  $ cat >some/dir/src.c <<EOF
  > #include <caml/mlvalues.h>
  > #include "header.h"
  > #include "correction.h"
  > value answer(value unit) { return Val_int((ANSWER + CORRECTION) * 2); }
  > EOF

  $ export OCAMLPATH=$PWD/external/install/lib; dune exec ./main.exe --root=some/dir
  Entering directory 'some/dir'
  Leaving directory 'some/dir'
  Answer = 42

----------------------------------------------------------------------------------
* External library directories in (include_dir ...)
* Build fails when using an unknown library

  $ cat >some/dir/dune <<EOF
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
  $ dune exec some/dir/main.exe
  File "some/dir/dune", line 4, characters 33-44:
  4 |  (include_dirs (lib answer) (lib unknown_lib))
                                       ^^^^^^^^^^^
  Error: Library "unknown_lib" not found.
  -> required by _build/default/some/dir/src.o
  -> required by _build/default/some/dir/libclib.a
  -> required by _build/default/some/dir/main.exe
  [1]

----------------------------------------------------------------------------------
* Error when using path separators in an archive name

  $ mkdir -p github2914/dir
  $ echo "(lang dune 2.1)" > github2914/dir/dune-project
  $ cat >github2914/dir/dune <<EOF
  > (foreign_library
  >  (archive_name some/path/id)
  >  (language c)
  >  (names src))
  > EOF

  $ dune build --root=github2914/dir
  Entering directory 'github2914/dir'
  File "dune", line 2, characters 15-27:
  2 |  (archive_name some/path/id)
                     ^^^^^^^^^^^^
  Error: Path separators are not allowed in archive names.
  Leaving directory 'github2914/dir'
  [1]

----------------------------------------------------------------------------------
* Using (foreign_archives dir/id) in a (library ...), see #2914

  $ mkdir -p github2914/dir

  $ cat >github2914/dir/dune <<EOF
  > (foreign_library
  >  (archive_name id)
  >  (language c)
  >  (names src))
  > EOF

  $ cat >github2914/dir/src.c <<EOF
  > #include <caml/mlvalues.h>
  > value id(value unit) { return Val_int(2914); }
  > EOF

  $ cat >github2914/dune <<EOF
  > (library
  >  (name bug)
  >  (foreign_archives dir/id)
  >  (modules bug))
  > (executable
  >  (name main)
  >  (modes byte exe)
  >  (libraries bug)
  >  (modules main))
  > EOF

  $ cat >github2914/bug.ml <<EOF
  > external id : unit -> int = "id"
  > let fix = Printf.sprintf "Bug #%d has been fixed" (id ())
  > EOF

  $ cat >github2914/ticket.mli <<EOF
  > val fix : string
  > EOF

  $ cat >github2914/main.ml <<EOF
  > let () = Printf.printf "%s\n" Bug.fix;
  > EOF

  $ dune exec github2914/main.exe
  Bug #2914 has been fixed

  $ dune build @github2914/all
  $ (cd _build/default/github2914 && ocamlrun -I dir main.bc)
  Bug #2914 has been fixed

----------------------------------------------------------------------------------
* Make sure the [Byte_with_stubs_statically_linked_in] mode works too

  $ cat >dune-workspace <<EOF
  > (lang dune 2.0)
  > (context
  >   (default (disable_dynamically_linked_foreign_archives true)))
  > EOF

  $ cat >github2914/dune <<EOF
  > (library
  >  (name bug)
  >  (foreign_archives dir/id)
  >  (modules bug))
  > (executable
  >  (name main)
  >  (modes byte)
  >  (libraries bug)
  >  (modules main))
  > EOF

  $ dune clean
  $ dune exec github2914/main.exe
  Bug #2914 has been fixed

--------------------------------------------------------------------------------
* Make sure that foreign stubs referencing code in foreign libs works too

  $ mkdir -p stubs_in_libs
  $ cat >stubs_in_libs/dune-project <<EOF
  > (lang dune 2.5)
  > EOF
  $ cat >stubs_in_libs/lib.h <<EOF
  > extern int foo(void);
  > EOF
  $ cat >stubs_in_libs/lib.c <<EOF
  > int foo(void) { return 12; }
  > EOF
  $ cat >stubs_in_libs/stubs.c <<EOF
  > #include <caml/mlvalues.h>
  > #include "lib.h"
  > value bar(value unit) { return Val_int(foo()); }
  > EOF
  $ cat >stubs_in_libs/main.ml <<EOF
  > external bar: unit -> int = "bar" [@@noalloc]
  > let () = print_endline (string_of_int (bar ()))
  > EOF
  $ cat >stubs_in_libs/dune <<EOF
  > (foreign_library
  >  (archive_name lib)
  >  (language c)
  >  (names lib))
  > 
  > (executable
  >  (name main)
  >  (foreign_archives lib)
  >  (foreign_stubs (language c) (names stubs)))
  > EOF
  $ dune build --root stubs_in_libs
  Entering directory 'stubs_in_libs'
  Leaving directory 'stubs_in_libs'
  $ stubs_in_libs/_build/default/main.exe
  12

--------------------------------------------------------------------------------
* Test (enabled_if)

  $ mkdir -p enabled_if
  $ cat >enabled_if/dune-project <<EOF
  > (lang dune 3.13)
  > EOF
  $ cat >enabled_if/lib.c <<EOF
  > int foo(void) { return 0; }
  > EOF
  $ cat >enabled_if/dune <<EOF
  > (foreign_library
  >  (archive_name lib)
  >  (language c)
  >  (enabled_if (<> %{env:ENABLE=} "0"))
  >  (names lib))
  > EOF
  $ dune build --root enabled_if
  Entering directory 'enabled_if'
  File "dune", line 4, characters 1-37:
  4 |  (enabled_if (<> %{env:ENABLE=} "0"))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'enabled_if' is only available since version 3.14 of the dune
  language. Please update your dune-project file to have (lang dune 3.14).
  Leaving directory 'enabled_if'
  [1]
  $ cat >enabled_if/dune-project <<EOF
  > (lang dune 3.14)
  > EOF
  $ cat >>enabled_if/dune <<EOF
  > (library
  >  (name lib)
  >  (foreign_archives lib))
  > EOF
  $ ENABLE=1 dune build --root enabled_if
  Entering directory 'enabled_if'
  Leaving directory 'enabled_if'
  $ ENABLE=0 dune build --root enabled_if
  Entering directory 'enabled_if'
  File "dune", lines 6-8, characters 0-45:
  6 | (library
  7 |  (name lib)
  8 |  (foreign_archives lib))
  Error: No rule found for liblib.a
  Leaving directory 'enabled_if'
  [1]
