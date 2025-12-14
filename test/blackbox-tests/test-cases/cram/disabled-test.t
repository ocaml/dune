Test behavior of disabled cram tests.

The enabled_if field controls whether a test is included in @runtest.
Currently, disabled tests silently succeed when run explicitly - the test
simply doesn't run but no error is reported.

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

Running the disabled test explicitly also silently succeeds (no diff shown):

  $ dune runtest disabled.t
