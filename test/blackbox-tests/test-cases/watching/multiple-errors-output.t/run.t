We test the output of the watch mode client when we have multiple errors

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
  File "$TESTCASE_ROOT/src/foo.ml", line 1, characters 39-40:
  1 | let f = Bar.x + Baz.y + invalid_syntax : ? = !
                                             ^
  Syntax error
  Error: Build failed with 3 errors.
  [1]

  $ stop_dune
  File "src/foo.ml", line 1, characters 39-40:
  1 | let f = Bar.x + Baz.y + invalid_syntax : ? = !
                                             ^
  Error: Syntax error
  File "libs/bar.ml", line 1, characters 8-20:
  1 | let y = "type error" + 3
              ^^^^^^^^^^^^
  Error: This constant has type string but an expression was expected of type
           int
  File "libs/baz.ml", line 1, characters 29-33:
  1 | let y = "unknown variable" ^ what
                                   ^^^^
  Error: Unbound value what
  Had 3 errors, waiting for filesystem changes...
