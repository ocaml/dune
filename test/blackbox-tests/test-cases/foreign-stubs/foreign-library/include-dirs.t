Testsuite for library directories in (include_dirs ...).

  $ setup_foreign_library_project

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
  Answer = 42

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
