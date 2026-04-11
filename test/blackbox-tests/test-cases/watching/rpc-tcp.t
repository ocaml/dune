Test that the RPC server can listen on TCP when DUNE_CONFIG__RPC_ADDRESS is set.

  $ echo '(lang dune 3.21)' > dune-project
  $ touch foo.ml

  $ export DUNE_CONFIG__RPC_ADDRESS='tcp:host=127.0.0.1,port=8587'

  $ start_dune

  $ dune rpc ping --wait
  Server appears to be responding normally

Verify that the RPC address file contains a TCP address, not a Unix socket.

  $ cat _build/.rpc/dune
  tcp:host=127.0.0.1,port=8587

  $ stop_dune

Invalid addresses should be rejected.

  $ DUNE_CONFIG__RPC_ADDRESS='garbage' dune build
  Error: Invalid value for "DUNE_CONFIG__RPC_ADDRESS"
  Invalid RPC address: invalid address format garbage
  [1]

Pinging the wrong port should fail.

  $ DUNE_CONFIG__RPC_ADDRESS='tcp:host=127.0.0.1,port=9999' dune rpc ping
  Error: RPC server not running.
  [1]
