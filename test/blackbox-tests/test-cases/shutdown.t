Check that `dune shutdown` behaves as intended.

  $ cat > dune-project <<EOF
  > (lang dune 2.8)
  > EOF

  $ cat > dune <<EOF
  > EOF

Start RPC server

  $ dune build -w > /dev/null &
  $ SERVER_PID=$!

Shutdown the build

  $ dune shutdown

Ensure that dune is closed

  $ ps -p $SERVER_PID
  PID TTY        TIME CMD
  [1]
