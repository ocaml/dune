This shows that the test suite is impacted by CLICOLOR_FORCE=1 (#6607).
This environment variable should only impact the output of dune, not behavior
visible from within the test suite.

  $ cat > dune-project << EOF
  > (lang dune 1.0)
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name l)
  >  (libraries stdune)
  >  (preprocess (pps ppx_expect))
  >  (inline_tests))
  > EOF

  $ cat > l.ml << EOF
  > open[@ocaml.alert "-unstable"] Stdune
  > 
  > let%expect_test _ =
  >   User_message.print (User_error.make []);
  >   [%expect]
  > EOF

When running with `CLICOLOR_FORCE=0`, no escape codes are present in the output
string. But when it is set to `1`, escape codes are present.
Remarks:
- it is necessary to pipe into `tr` to see the difference because otherwise these
escape codes are stripped by cram, and it is necessary
- it is necessary to call `dune clean` because `dune` does not know that the
runtest depends on `CLICOLOR_FORCE`.

The expected outcome would be that both of these runs output the same corrected
file.

  $ CLICOLOR_FORCE=0 dune runtest --auto-promote
  File "l.ml", line 1, characters 0-0:
  Error: Files _build/default/l.ml and _build/default/l.ml.corrected differ.
  Promoting _build/default/l.ml.corrected to l.ml.
  [1]
  $ < l.ml tr '\033' '?'
  open[@ocaml.alert "-unstable"] Stdune
  
  let%expect_test _ =
    User_message.print (User_error.make []);
    [%expect{| Error: |}]
  $ dune clean
  $ CLICOLOR_FORCE=1 dune runtest --auto-promote
  File "l.ml", line 1, characters 0-0:
  Error: Files _build/default/l.ml and _build/default/l.ml.corrected differ.
  Promoting _build/default/l.ml.corrected to l.ml.
  [1]
  $ < l.ml tr '\033' '?'
  open[@ocaml.alert "-unstable"] Stdune
  
  let%expect_test _ =
    User_message.print (User_error.make []);
    [%expect{| ?[1;31mError?[0m: |}]
