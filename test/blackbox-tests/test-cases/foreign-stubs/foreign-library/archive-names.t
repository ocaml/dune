Testsuite for foreign archive names and nested archives.

  $ setup_foreign_library_project

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
