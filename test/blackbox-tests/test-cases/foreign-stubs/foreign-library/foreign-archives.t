Testsuite for interactions between (foreign_archives ...) and other stanzas.

  $ setup_foreign_library_project
  $ write_add_mul_sources
  $ write_headers_config_sources

* Interaction of (foreign_stubs ...) and (foreign_archives ...).

  $ write_calc_with_month_headers_config_dune
  $ write_month_calc_project

  $ rm -rf _build
  $ dune build

  $ dune exec ./main.exe
  October 2019

  $ (cd _build/default && ocamlrun -I lib main.bc)
  October 2019

* Error when using (foreign_archives ...) and a pure bytecode (executable ...).

  $ write_calc_with_month_headers_config_dune with-day

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (modes exe byte)
  >  (libraries calc)
  >  (foreign_archives lib/day)
  >  (modules main))
  > EOF

  $ write_day_source
  $ write_day_main

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

* Interaction of (foreign_archives ...) and (executables ...).
* Foreign archives in subdirectories.

  $ write_calc_with_month_headers_config_dune with-day

  $ cat >dune <<EOF
  > (executable
  >  (modes exe)
  >  (name main)
  >  (libraries calc)
  >  (foreign_archives lib/day)
  >  (modules main))
  > EOF

  $ write_day_source
  $ write_day_main

  $ rm -rf _build
  $ dune build

  $ dune exec ./main.exe
  8 October 2019

* Use (env ...) to pass C++ flags.

  $ write_calc_with_month_dune with-day <<EOF
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (include_dirs headers)
  >  (extra_deps eight.h)
  >  (names config))
  > EOF

  $ cat >dune <<EOF
  > (env (_ (cxx_flags :standard -DCONFIG_VALUE=2000)))
  > (executable
  >  (modes exe)
  >  (name main)
  >  (libraries calc)
  >  (foreign_archives lib/day)
  >  (modules main))
  > EOF

  $ dune exec ./main.exe
  8 October 2019

* Generated header.

  $ write_calc_with_month_dune with-day <<EOF
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (include_dirs headers)
  >  (flags :standard -DCONFIG_VALUE=2000)
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
