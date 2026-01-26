Demonstrate running "dune runtest" concurrently with an eager rpc server.

  $ echo '(lang dune 3.18)' > dune-project

Define a test that just prints "Hello, World!"
  $ cat > dune << 'EOF'
  > (rule
  >  (alias runtest)
  >  (action (echo "Hello, World!")))
  > EOF

  $ dune build --watch &
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Hello, World!

Make sure the RPC server is properly started:
  $ dune rpc ping --wait
  Server appears to be responding normally

Test that we can run a test while another instance of dune is running in watch
mode:
  $ dune runtest 2>&1
  Success

  $ dune shutdown
  $ wait
