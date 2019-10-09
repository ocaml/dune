----------------------------------------------------------------------------------
Testsuite for the (foreign_stubs ...) stanza.

----------------------------------------------------------------------------------
* Error when using both (self_build_stubs_archive ...) and (c_names ...) before 2.0.

  $ echo "(lang dune 1.0)" > dune-project

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (c_names foo)
  >  (self_build_stubs_archive (bar)))
  > EOF

  $ dune build
  File "dune", line 4, characters 1-33:
  4 |  (self_build_stubs_archive (bar)))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: A library cannot use (self_build_stubs_archive ...) and (c_names ...)
  simultaneously. This is supported starting from Dune 2.0.
  [1]

----------------------------------------------------------------------------------
* Warning when using (self_build_stubs_archive ...) in Dune 2.0.
* Warning when using (c_names ...) in Dune 2.0.
* Error when a C source file is missing.

  $ echo "(lang dune 2.0)" > dune-project

  $ dune build
  File "dune", line 3, characters 1-14:
  3 |  (c_names foo)
       ^^^^^^^^^^^^^
  Warning: 'c_names' was deprecated in version 2.0 of the dune language. Use
  the (foreign_stubs ...) stanza instead.
  File "dune", line 4, characters 1-33:
  4 |  (self_build_stubs_archive (bar)))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: 'self_build_stubs_archive' was deprecated in version 2.0 of the dune
  language. Use the (foreign_stubs_archives ...) stanza instead.
  File "dune", line 3, characters 10-13:
  3 |  (c_names foo)
                ^^^
  Error: Object "foo" has no source; "foo.c" must be present.
  [1]

----------------------------------------------------------------------------------
* Warning when using (self_build_stubs_archive ...) in Dune 2.0.
* Error when a self-built archive is missing.

  $ cat >foo.c <<EOF
  > #include <caml/mlvalues.h>
  > value foo() { return Val_int(9); }
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (foreign_stubs (language c) (names foo))
  >  (self_build_stubs_archive (bar)))
  > EOF

  $ dune build
  File "dune", line 4, characters 1-33:
  4 |  (self_build_stubs_archive (bar)))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: 'self_build_stubs_archive' was deprecated in version 2.0 of the dune
  language. Use the (foreign_stubs_archives ...) stanza instead.
  Error: No rule found for libbar_stubs$ext_lib
  [1]

----------------------------------------------------------------------------------
* Warning when using (self_build_stubs_archive ...) in Dune 2.0.
* Build succeeds when a self-built archive exists.

  $ cat >bar.c <<EOF
  > #include <caml/mlvalues.h>
  > value bar() { return Val_int(10); }
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (foreign_stubs (language c) (names foo))
  >  (self_build_stubs_archive (bar)))
  > (rule
  >  (targets bar%{ext_obj})
  >  (deps bar.c)
  >  (action (run %{ocaml-config:c_compiler} -c -I %{ocaml-config:standard_library} -o %{targets} %{deps})))
  > (rule
  >  (targets libbar_stubs.a)
  >  (deps bar%{ext_obj})
  >  (action (run ar rcs %{targets} %{deps})))
  > EOF

  $ dune build
  File "dune", line 4, characters 1-33:
  4 |  (self_build_stubs_archive (bar)))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: 'self_build_stubs_archive' was deprecated in version 2.0 of the dune
  language. Use the (foreign_stubs_archives ...) stanza instead.

----------------------------------------------------------------------------------
* Error when specifying an (archive_name ...) in (foreign_stubs ...) stanza.

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (foreign_stubs (archive_name baz) (language c) (names foo))
  >  (self_build_stubs_archive (bar)))
  > (rule
  >  (targets bar%{ext_obj})
  >  (deps bar.c)
  >  (action (run %{ocaml-config:c_compiler} -c -I %{ocaml-config:standard_library} -o %{targets} %{deps})))
  > (rule
  >  (targets libbar_stubs.a)
  >  (deps bar%{ext_obj})
  >  (action (run ar rcs %{targets} %{deps})))
  > EOF

  $ dune build
  File "dune", line 3, characters 16-34:
  3 |  (foreign_stubs (archive_name baz) (language c) (names foo))
                      ^^^^^^^^^^^^^^^^^^
  Error: The field "archive_name" is disallowed in the (foreign_stubs ...)
  stanza. For named foreign archives use the (foreign_library ...) stanza.
  [1]

----------------------------------------------------------------------------------
* Foreign stubs in C and C++ language.
* Multiple foreign stub archives.

  $ cat >baz.cpp <<EOF
  > #include <caml/mlvalues.h>
  > extern "C" value baz() { return Val_int(0); }
  > EOF

  $ cat >qux.cpp <<EOF
  > #include <caml/mlvalues.h>
  > extern "C" value qux() { return Val_int(2000); }
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
  >  (foreign_stubs_archives bar qux)
  >  (foreign_stubs (language cxx) (names baz)))
  > (rule
  >  (targets bar%{ext_obj})
  >  (deps bar.c)
  >  (action (run %{ocaml-config:c_compiler} -c -I %{ocaml-config:standard_library} -o %{targets} %{deps})))
  > (rule
  >  (targets libbar.a)
  >  (deps bar%{ext_obj})
  >  (action (run ar rcs %{targets} %{deps})))
  > (rule
  >  (targets dllbar%{ext_dll})
  >  (deps bar%{ext_obj})
  >  (action (run %{ocaml-config:c_compiler} -shared -o %{targets} %{deps})))
  > (rule
  >  (targets qux%{ext_obj})
  >  (deps qux.cpp)
  >  (action (run %{ocaml-config:c_compiler} -c -I %{ocaml-config:standard_library} -o %{targets} %{deps})))
  > (rule
  >  (targets libqux.a)
  >  (deps qux%{ext_obj})
  >  (action (run ar rcs %{targets} %{deps})))
  > (rule
  >  (targets dllqux%{ext_dll})
  >  (deps qux%{ext_obj})
  >  (action (run %{ocaml-config:c_compiler} -shared -o %{targets} %{deps})))
  > (executable
  >  (name main)
  >  (libraries quad)
  >  (modules main))
  > EOF

  $ dune build --display short
           gcc baz$ext_obj
    ocamlmklib dllquad_stubs$ext_dll,libquad_stubs$ext_lib
           gcc dllbar$ext_dll
           gcc qux$ext_obj
            ar libqux$ext_lib
            ar libbar$ext_lib
      ocamldep .quad.objs/quad.mli.d
        ocamlc .quad.objs/byte/quad.{cmi,cmti}
      ocamldep .main.eobjs/main.ml.d
        ocamlc .main.eobjs/byte/dune__exe__Main.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/native/dune__exe__Main.{cmx,o}
      ocamldep .quad.objs/quad.ml.d
      ocamlopt .quad.objs/native/quad.{cmx,o}
      ocamlopt quad.{a,cmxa}
      ocamlopt quad.cmxs
           gcc dllqux$ext_dll
        ocamlc .quad.objs/byte/quad.{cmo,cmt}
        ocamlc quad.cma
        ocamlc main.bc
      ocamlopt main.exe

  $ dune exec ./main.exe
  2019

  $ (cd _build/default && ocamlrun -I . ./main.bc)
  2019
