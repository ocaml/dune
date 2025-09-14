We test the output of the watch mode client when we have multiple errors

  $ . ../helpers.sh

  $ start_dune

  $ dune rpc ping --wait
  Server appears to be responding normally

  $ dune build
  File "$TESTCASE_ROOT/libs/bar.ml", line 1, characters 8-20:
  1 | let y = "type error" + 3
              ^^^^^^^^^^^^
  This constant has type string but an expression was expected of type
    int
  File "$TESTCASE_ROOT/libs/baz.ml", line 1, characters 29-33:
  1 | let y = "unknown variable" ^ what
                                   ^^^^
  Unbound value what
  Build failed with 2 errors.

  $ stop_dune
  File "libs/bar.ml", line 1, characters 8-20:
  1 | let y = "type error" + 3
              ^^^^^^^^^^^^
  Error: This constant has type string but an expression was expected of type
           int
  File "libs/baz.ml", line 1, characters 29-33:
  1 | let y = "unknown variable" ^ what
                                   ^^^^
  Error: Unbound value what
  Had 2 errors, waiting for filesystem changes...
