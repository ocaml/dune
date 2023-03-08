Tests for how error messages are displayed
==========================================

The purpose of these tests is to check that errors are displayed with
a separating blank line (issue #6158, PR #6823).

Test setup
----------

  $ cat >dune-project <<EOF
  > (lang dune 2.3)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name lib))
  > EOF

  $ cat >a.ml <<EOF
  > let f x y z = ()
  > (* this should produce 3 warnings for unused variables *)
  > EOF

  $ cat >b.ml <<EOF
  > let () = 1
  > (* this should produce a type error *)
  > EOF

  $ cat >c.ml <<EOF
  > let x = +
  > (* this should produce a syntax error *)
  > EOF

Actual tests
------------

We check that the errors reported for different files are separated by
blank lines. If a file generates several errors (which is the case for
the `a.ml` file, then no blank lines are inserted between them,
because this is the exact message that is reported by the Ocaml
compiler, and we do not parse or modify such messages.

Without the --display-separate-messages flag, no blank line is put
between error messages for different files, as expected.

  $ dune build
  File "c.ml", line 3, characters 0-0:
  Error: Syntax error
  File "a.ml", line 1, characters 6-7:
  1 | let f x y z = ()
            ^
  Error (warning 27 [unused-var-strict]): unused variable x.
  File "a.ml", line 1, characters 8-9:
  1 | let f x y z = ()
              ^
  Error (warning 27 [unused-var-strict]): unused variable y.
  File "a.ml", line 1, characters 10-11:
  1 | let f x y z = ()
                ^
  Error (warning 27 [unused-var-strict]): unused variable z.
  File "b.ml", line 1, characters 9-10:
  1 | let () = 1
               ^
  Error: This expression has type int but an expression was expected of type
           unit
  [1]

With the --display-separate-messages flag, a blank line is put between
error messages for different files. No blank line is inserted before
the first message, and no blank line is inserted after the last
message either.

  $ dune build --display-separate-messages
  File "c.ml", line 3, characters 0-0:
  Error: Syntax error
  
  File "a.ml", line 1, characters 6-7:
  1 | let f x y z = ()
            ^
  Error (warning 27 [unused-var-strict]): unused variable x.
  File "a.ml", line 1, characters 8-9:
  1 | let f x y z = ()
              ^
  Error (warning 27 [unused-var-strict]): unused variable y.
  File "a.ml", line 1, characters 10-11:
  1 | let f x y z = ()
                ^
  Error (warning 27 [unused-var-strict]): unused variable z.
  
  File "b.ml", line 1, characters 9-10:
  1 | let () = 1
               ^
  Error: This expression has type int but an expression was expected of type
           unit
  [1]
