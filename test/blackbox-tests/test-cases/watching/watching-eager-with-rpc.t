Demonstrate building through rpc when server is running in eager watch

  $ echo '(lang dune 3.8)' > dune-project
  $ mkdir src
  $ echo '(library (name foo))' > src/dune
  $ touch src/a.ml
  $ touch src/b.ml

  $ dune build @all
  $ dune build --watch > .#dune-output 2>&1 &
  $ DUNE_PID=$!
  $ wait_for_rpc_server

  $ build .
  Success

  $ stop_dune_quiet
