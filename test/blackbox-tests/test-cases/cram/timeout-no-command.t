Testing that timeout errors include the partial output from the command that
caused the timeout.

  $ make_dune_project 3.20

  $ cat > dune <<EOF
  > (cram
  >  (timeout 0.5))
  > EOF

Create a cram test with multiple commands, where the second one blocks until
the timeout:

  $ cat > test.t <<EOF
  >   $ echo "This command runs fine"
  >   $ echo "This is the problematic command"
  >   > rm -f block
  >   > mkfifo block
  >   > cat block
  > EOF

Run the test and verify that the timeout output includes the partial output of
the command that timed out:

  $ timeout 3 dune test test.t
  File "test.t", line 1, characters 0-0:
  --- test.t
  +++ test.t.corrected
  @@ -1,5 +1,8 @@
     $ echo "This command runs fine"
  +  This command runs fine
     $ echo "This is the problematic command"
     > rm -f block
     > mkfifo block
     > cat block
  +  This is the problematic command
  +  [timed out]
  File "test.t", line 1, characters 0-0:
  Error: Cram test timed out
  A time limit of 0.50s has been set in dune:2
  [1]
