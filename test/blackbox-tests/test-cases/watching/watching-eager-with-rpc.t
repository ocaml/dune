Demonstrate building through rpc when server is running in eager watch

  $ echo '(lang dune 3.8)' > dune-project
  $ mkdir src
  $ echo '(library (name foo))' > src/dune
  $ touch src/a.ml
  $ touch src/b.ml

  $ dune build @all
  $ dune build --watch > .#dune-output 2>&1 &
  $ wait_for_rpc_server

  $ dune rpc build --wait .
  Success

  $ dune shutdown
  $ wait
  $ grep -m1 "Success, waiting" .#dune-output
  Success, waiting for filesystem changes...
