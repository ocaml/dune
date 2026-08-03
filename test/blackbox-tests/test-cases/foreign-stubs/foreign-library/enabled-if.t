Test (enabled_if) for foreign libraries.

  $ setup_foreign_library_project

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
  $ ENABLE=0 dune build --root enabled_if
  Entering directory 'enabled_if'
  File "dune", lines 6-8, characters 0-45:
  6 | (library
  7 |  (name lib)
  8 |  (foreign_archives lib))
  Error: No rule found for liblib.a
  Leaving directory 'enabled_if'
  [1]
