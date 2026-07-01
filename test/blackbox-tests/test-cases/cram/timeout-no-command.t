Testing that timeout errors include the partial output from the command that
caused the timeout.

  $ make_dune_project 3.20

  $ cat > dune <<EOF
  > (cram
  >  (timeout 0.5))
  > EOF

Create a cram test with multiple commands, where the second one will timeout:

  $ cat > test.t <<EOF
  >   $ echo "This command runs fine"
  >   $ echo "This is the problematic command" && sleep 2
  > EOF

Run the test and verify that the timeout output includes the partial output of
the command that timed out:

  $ dune test test.t
  File "test.t", line 1, characters 0-0:
  --- test.t
  +++ test.t.corrected
  @@ -1,2 +1,5 @@
     $ echo "This command runs fine"
  +  This command runs fine
     $ echo "This is the problematic command" && sleep 2
  +  This is the problematic command
  +  [timed out]
  File "test.t", line 1, characters 0-0:
  Error: Cram test timed out
  A time limit of 0.50s has been set in dune:2
  [1]
