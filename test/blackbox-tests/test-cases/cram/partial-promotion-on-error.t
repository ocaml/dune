Test that cram tests with unreachable commands can still partially promote
output from commands that executed successfully.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

Create a test with some successful commands, then an exit, then unreachable
commands:

  $ cat > test.t <<EOF
  >   $ echo "First command"
  >   $ echo "Second command"
  >   $ exit 1
  >   $ echo "This will never execute"
  >   $ echo "Neither will this"
  > EOF

The test should fail but still capture output from executed commands:
  $ dune runtest
  File "test.t", line 1, characters 0-0:
  Error: Files _build/default/test.t and _build/default/test.t.corrected
  differ.
  [1]
  $ dune promote
  Promoting _build/default/test.t.corrected to test.t.

After promotion, the test file should have output from successful commands and
UNREACHABLE markers for the rest:
  $ cat test.t
    $ echo "First command"
    First command
    $ echo "Second command"
    Second command
    $ exit 1
    ***** UNREACHABLE *****
    $ echo "This will never execute"
    ***** UNREACHABLE *****
    $ echo "Neither will this"
    ***** UNREACHABLE *****

