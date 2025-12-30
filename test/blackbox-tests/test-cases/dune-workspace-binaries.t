Demonstrate a failure of dune to resolve a binary added via binaries in
a dune-workspace file:

  $ cat >dune-workspace <<EOF
  > (lang dune 3.21)
  > 
  > (env
  >  (_
  >   (binaries (test.sh as foobar))
  >   (flags :standard)))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (target message.txt)
  >  (action (with-stdout-to %{target} (run foobar))))
  > EOF

  $ cat >test.sh <<EOF
  > #!/bin/sh
  > echo "This is only a test."
  > EOF

  $ chmod +x test.sh
  $ dune build ./message.txt
