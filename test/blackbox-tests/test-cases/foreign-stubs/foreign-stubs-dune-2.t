----------------------------------------------------------------------------------
Compatability testsuite for the (foreign_stubs ...) field.

  $ ./sandboxed.sh

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
* Error when using (c_names ...) in (library ...) in Dune 2.0.

  $ echo "(lang dune 2.0)" > dune-project

  $ dune build
  File "dune", line 3, characters 1-14:
  3 |  (c_names foo)
       ^^^^^^^^^^^^^
  Error: 'c_names' was deleted in version 2.0 of the dune language. Use the
  (foreign_stubs ...) field instead.
  [1]

----------------------------------------------------------------------------------
* Error when using (c_names ...) in (executable ...) in Dune 2.0.

  $ cat >dune <<EOF
  > (executable
  >  (name foo)
  >  (c_names bar))
  > EOF

  $ dune build
  File "dune", line 3, characters 2-9:
  3 |  (c_names bar))
        ^^^^^^^
  Error: Unknown field "c_names"
  [1]

----------------------------------------------------------------------------------
* Error when using (self_build_stubs_archive ...) in (library ...) in Dune 2.0.

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
  Error: 'self_build_stubs_archive' was deleted in version 2.0 of the dune
  language. Use the (foreign_archives ...) field instead.
  [1]

----------------------------------------------------------------------------------
* Error when using (self_build_stubs_archive ...) in (executable ...) in Dune 2.0.

  $ cat >dune <<EOF
  > (executable
  >  (name foo)
  >  (foreign_stubs (language c) (names bar))
  >  (self_build_stubs_archive (baz)))
  > EOF

  $ dune build
  File "dune", line 4, characters 2-26:
  4 |  (self_build_stubs_archive (baz)))
        ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unknown field "self_build_stubs_archive"
  [1]

