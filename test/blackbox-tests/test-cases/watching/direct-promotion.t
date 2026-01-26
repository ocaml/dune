Demonstrate running "dune promote" concurrently with a passive rpc server

  $ echo '(lang dune 3.20)' > dune-project
  $ echo "  $ echo hello" > my_test.t

  $ start_dune

The test expectedly fails (see errors at the end of this file)
  $ build "(alias my_test)"
  Failure

Promotion happens on the running RPC server.
  $ dune promote
  Success

  $ build "(alias my_test)"
  Success

  $ stop_dune
  File "my_test.t", line 1, characters 0-0:
  --- my_test.t
  +++ my_test.t.corrected
  @@ -1 +1,2 @@
     $ echo hello
  +  hello
  Had 1 error, waiting for filesystem changes...
  Promoting _build/default/my_test.t.corrected to my_test.t.
  Success, waiting for filesystem changes...
