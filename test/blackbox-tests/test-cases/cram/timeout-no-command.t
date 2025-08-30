Testing that timeout errors don't include the command that caused the timeout.

This test demonstrates the current behavior where timeout error messages
don't include information about which specific command caused the timeout.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat > dune <<EOF
  > (cram
  >  (timeout 0.1))
  > EOF

Create a cram test with multiple commands, where the second one will timeout:

  $ cat > test.t <<EOF
  >   $ echo "This command runs fine"
  >   $ echo "This is the problematic command" && sleep 2
  > EOF

Run the test and verify that the timeout error doesn't mention 
which specific command caused the timeout:

  $ dune test test.t
  File "test.t", line 1, characters 0-0:
  Error: Cram test timed out while running command:
    $ echo "This is the problematic command" && sleep 2
  A time limit of 0.10s has been set in dune:2
  [1]

The error message above shows that we get a generic timeout message but no
indication that it was the "echo && sleep 2" command that caused the timeout.
This makes debugging timeout issues difficult when there are multiple commands
in a test.
