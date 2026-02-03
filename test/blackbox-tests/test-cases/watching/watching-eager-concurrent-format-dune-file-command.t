Demonstrate running "dune format-dune-file" concurrently with an eager rpc server.

  $ echo '(lang dune 3.18)' > dune-project

Define a simple rule so the build succeeds:
  $ cat > dune << 'EOF'
  > (rule
  >  (alias default)
  >  (action (echo "Building...")))
  > EOF

  $ dune build --watch &
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Building...

Make sure the RPC server is properly started:
  $ dune rpc ping --wait
  Server appears to be responding normally

Test that we can format a file while another instance of dune is running in watch
mode:
  $ echo '(   library    (name    foo)   )' | dune format-dune-file
  (library
   (name foo))

Test formatting a file path:
  $ echo '(a     b     c)' > test.dune
  $ dune format-dune-file test.dune
  (a b c)

  $ dune shutdown
  $ wait
