Testing the detection of the --explain flag when executing the menhir rules.

The menhir rules check for the --explain flag in a menhir stanza or any provided
via an env stanza using the menhir_flags field.

Build and run a source file that requires a menhir parser.

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (using menhir 2.1)
  > EOF

  $ dune build ./src/test.exe
  $ ls _build/default/src/test.exe
  _build/default/src/test.exe

Check for .conflicts files in stanza with --explain. This will fail because the
menhir lang version is not 2.2 or above.

  $ find _build/default/src -name '*.conflicts'

Try building again with 2.2

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (using menhir 2.2)
  > EOF
  $ dune build ./src/test.exe

Check for .conflicts files in stanza with --explain.

  $ find _build/default/src -name '*.conflicts'
  _build/default/src/test_base.conflicts

Now we test for --explain provided via an env stanza:

  $ cat > dune << EOF
  > (env (_ (menhir_flags :standard "--explain")))
  > EOF

Passing --explain through menhir_flags is prohibited for the moment.

  $ dune build ./src/test.exe
  File "dune", line 1, characters 32-43:
  1 | (env (_ (menhir_flags :standard "--explain")))
                                      ^^^^^^^^^^^
  Error: The --explain flag is not supported in the env stanza as it currently
  does not allow Dune's menhir support to produce the correct targets.
  Hint: Add --explain to the (flags) field of the (menhir) stanza. This will
  produce the correct .conflict files.
  [1]
