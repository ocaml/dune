Test that disabled cram tests can still be run explicitly.

The enabled_if field controls whether a test is included in @runtest,
but does not prevent the test from being run explicitly.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat > dune <<EOF
  > (cram
  >  (applies_to disabled)
  >  (enabled_if false))
  > EOF

  $ cat > disabled.t <<EOF
  >   $ echo "hello"
  >   wrong output
  > EOF

The disabled test is skipped from @runtest:

  $ dune runtest

But can be run explicitly (the diff proves it ran):

  $ dune runtest disabled.t
  File "disabled.t", line 1, characters 0-0:
  --- disabled.t
  +++ disabled.t.corrected
  @@ -1,2 +1,2 @@
     $ echo "hello"
  -  wrong output
  +  hello
  [1]
