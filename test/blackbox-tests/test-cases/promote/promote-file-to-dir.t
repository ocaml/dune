Demonstrate issue https://github.com/ocaml/dune/issues/7383 with changing a file for
promotion from a file to a directory whilst in the staging area. This causes Dune to get
confused and stuck.

Create a cram test:

  $ cat > dune-project << EOF
  > (lang dune 3.8)
  > EOF

  $ cat > my_cram.t << EOF
  >   $ echo hello
  > EOF

Run the cram test, should fail and file should be staged for promotion

  $ dune build @runtest
  File "my_cram.t", line 1, characters 0-0:
  Error: Files _build/default/my_cram.t and _build/default/my_cram.t.corrected
  differ.
  [1]

Change cram test to a directory cram test

  $ rm my_cram.t
  $ mkdir my_cram.t
  $ cat > my_cram.t/run.t << EOF
  >   $ echo hello
  > EOF

Run the cram test, should fail and file should be promoted

  $ dune build @runtest 2>&1 | sed -E 's#/.*.sandbox/[^/]+#/.sandbox/$SANDBOX#g'
  File "my_cram.t/run.t", line 1, characters 0-0:
  Error: Files _build/default/my_cram.t/run.t and
  _build/default/my_cram.t/run.t.corrected differ.
  Error:
  rename(_build/.sandbox/$SANDBOX/default/my_cram.t/run.t.corrected): Not a directory
  -> required by alias runtest

We cannot promote:

  $ dune promote
