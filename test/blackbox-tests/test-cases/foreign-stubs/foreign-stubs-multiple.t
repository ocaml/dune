Testsuite for the (foreign_stubs ...) field.

  $ echo "(lang dune 3.0)" > dune-project
  $ ./sandboxed.sh

----------------------------------------------------------------------------------
* Foreign stubs in C and C++ language.
* Multiple foreign stub archives.

  $ cat >baz.cpp <<EOF
  > #include <caml/mlvalues.h>
  > extern "C" value baz(value unit) { return Val_int(0); }
  > EOF

  $ cat >qux.cpp <<EOF
  > #include <caml/mlvalues.h>
  > extern "C" value qux(value unit) { return Val_int(2000); }
  > EOF
 
  $ cat >foo.c <<EOF
  > #include <caml/mlvalues.h>
  > value foo(value unit) { return Val_int(9); }
  > EOF
 
  $ cat >bar.c <<EOF
  > #include <caml/mlvalues.h>
  > value bar(value unit) { return Val_int(10); }
  > EOF

  $ cat >quad.ml <<EOF
  > external foo : unit -> int = "foo"
  > external bar : unit -> int = "bar"
  > external baz : unit -> int = "baz"
  > external qux : unit -> int = "qux"
  > let quad x = foo x + bar x + baz x + qux x
  > EOF

  $ cat >quad.mli <<EOF
  > val quad : unit -> int
  > EOF

  $ cat >main.ml <<EOF
  > let () = Printf.printf "%d" (Quad.quad ())
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name quad)
  >  (modules quad)
  >  (foreign_stubs (language c) (names foo))
  >  (foreign_archives bar qux)
  >  (foreign_stubs (language cxx) (names baz)))
  > (rule
  >  (targets bar%{ext_obj})
  >  (deps bar.c)
  >  (action (run %{cc} -c -I %{ocaml-config:standard_library} -o %{targets} %{deps})))
  > (rule
  >  (targets libbar.a)
  >  (deps bar%{ext_obj})
  >  (action (run ar rcs %{targets} %{deps})))
  > (rule
  >  (targets dllbar%{ext_dll})
  >  (deps bar%{ext_obj})
  >  (action (run %{cc} -shared -o %{targets} %{deps})))
  > (rule
  >  (targets qux%{ext_obj})
  >  (deps qux.cpp)
  >  (action (run %{cxx} -c -I %{ocaml-config:standard_library} -o %{targets} %{deps})))
  > (rule
  >  (targets libqux.a)
  >  (deps qux%{ext_obj})
  >  (action (run ar rcs %{targets} %{deps})))
  > (rule
  >  (targets dllqux%{ext_dll})
  >  (deps qux%{ext_obj})
  >  (action (run %{cc} -shared -o %{targets} %{deps})))
  > (executable
  >  (name main)
  >  (modes exe byte)
  >  (libraries quad)
  >  (modules main))
  > EOF

  $ dune build

  $ dune exec ./main.exe
  2019

  $ (cd _build/default && ocamlrun -I . ./main.bc)
  2019

----------------------------------------------------------------------------------
* Unused foreign sources are ignored.

  $ touch foo.cpp
  $ touch foo.cxx

  $ dune build

----------------------------------------------------------------------------------
* Fails when (extra_deps ...) is missing

  $ cat >foo.c <<EOF
  > #include <caml/mlvalues.h>
  > #include "eight.h"
  > value foo(value unit) { return Val_int(1 + EIGHT); }
  > EOF

  $ dune build 2> /dev/null
  [1]

----------------------------------------------------------------------------------
* Succeeds when (extra_deps ...) is present

  $ cat >eight.h <<EOF
  > #define EIGHT 8
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name quad)
  >  (modules quad)
  >  (foreign_stubs (language c) (names foo) (extra_deps eight.h))
  >  (foreign_archives bar qux)
  >  (foreign_stubs (language cxx) (names baz)))
  > (rule
  >  (targets bar%{ext_obj})
  >  (deps bar.c)
  >  (action (run %{cc} -c -I %{ocaml-config:standard_library} -o %{targets} %{deps})))
  > (rule
  >  (targets libbar.a)
  >  (deps bar%{ext_obj})
  >  (action (run ar rcs %{targets} %{deps})))
  > (rule
  >  (targets dllbar%{ext_dll})
  >  (deps bar%{ext_obj})
  >  (action (run %{cc} -shared -o %{targets} %{deps})))
  > (rule
  >  (targets qux%{ext_obj})
  >  (deps qux.cpp)
  >  (action (run %{cxx} -c -I %{ocaml-config:standard_library} -o %{targets} %{deps})))
  > (rule
  >  (targets libqux.a)
  >  (deps qux%{ext_obj})
  >  (action (run ar rcs %{targets} %{deps})))
  > (rule
  >  (targets dllqux%{ext_dll})
  >  (deps qux%{ext_obj})
  >  (action (run %{cc} -shared -o %{targets} %{deps})))
  > (executable
  >  (name main)
  >  (modes exe byte)
  >  (libraries quad)
  >  (modules main))
  > EOF

  $ dune exec ./main.exe
  2019

  $ (cd _build/default && ocamlrun -I . ./main.bc)
  2019

----------------------------------------------------------------------------------
* Fails when (include_dirs ...) is missing

  $ cat >foo.c <<EOF
  > #include <caml/mlvalues.h>
  > #include "eight.h"
  > #include "another/dir/one.h"
  > value foo(value unit) { return Val_int(ONE + EIGHT); }
  > EOF

  $ dune build 2> /dev/null
  [1]

----------------------------------------------------------------------------------
* Succeeds when (include_dirs ...) is present

  $ mkdir -p another/dir
  $ cat >another/dir/one.h <<EOF
  > #define ONE 1
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name quad)
  >  (modules quad)
  >  (foreign_stubs (language c) (names foo) (extra_deps eight.h) (include_dirs another/dir))
  >  (foreign_archives bar qux)
  >  (foreign_stubs (language cxx) (names baz)))
  > (rule
  >  (targets bar%{ext_obj})
  >  (deps bar.c)
  >  (action (run %{cc} -c -I %{ocaml-config:standard_library} -o %{targets} %{deps})))
  > (rule
  >  (targets libbar.a)
  >  (deps bar%{ext_obj})
  >  (action (run ar rcs %{targets} %{deps})))
  > (rule
  >  (targets dllbar%{ext_dll})
  >  (deps bar%{ext_obj})
  >  (action (run %{cc} -shared -o %{targets} %{deps})))
  > (rule
  >  (targets qux%{ext_obj})
  >  (deps qux.cpp)
  >  (action (run %{cxx} -c -I %{ocaml-config:standard_library} -o %{targets} %{deps})))
  > (rule
  >  (targets libqux.a)
  >  (deps qux%{ext_obj})
  >  (action (run ar rcs %{targets} %{deps})))
  > (rule
  >  (targets dllqux%{ext_dll})
  >  (deps qux%{ext_obj})
  >  (action (run %{cc} -shared -o %{targets} %{deps})))
  > (executable
  >  (name main)
  >  (modes exe byte)
  >  (libraries quad)
  >  (modules main))
  > EOF

  $ dune exec ./main.exe
  2019

  $ (cd _build/default && ocamlrun -I . ./main.bc)
  2019

----------------------------------------------------------------------------------
* Fails when using standard names in foreign stubs due to multiple C++ sources

  $ cat >dune <<EOF
  > (library
  >  (name quad)
  >  (modules quad)
  >  (foreign_stubs (language cxx)))
  > EOF

  $ dune build
  File "dune", line 4, characters 1-31:
  4 |  (foreign_stubs (language cxx)))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Multiple sources map to the same object name "foo":
  - foo.cpp
  - foo.cxx
  This is not allowed; please rename them or remove "foo" from object names.
  Hint: You can also avoid the name clash by placing the objects into different
  foreign archives and building them in different directories. Foreign archives
  can be defined using the (foreign_library ...) stanza.
  [1]

----------------------------------------------------------------------------------
* Succeeds when using a subset of standard names

  $ cat >dune <<EOF
  > (library
  >  (name quad)
  >  (modules quad)
  >  (foreign_stubs
  >   (language cxx)
  >   (names :standard \ foo)))
  > EOF

  $ dune clean
  $ dune build

