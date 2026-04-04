  $ make_dune_project 3.21

  $ cat >dune <<EOF
  > (env
  >  (_
  >   (binaries (test.sh as foobar))
  >   (flags :standard)))
  > 
  > (rule
  >  (target message.txt)
  >  (action (with-stdout-to %{target} (run foobar))))
  > EOF

  $ cat >test.sh <<EOF
  > #!/bin/sh
  > 
  > echo "This is only a test."
  > EOF

  $ chmod +x test.sh

  $ dune build ./message.txt
