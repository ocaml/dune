  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

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
  > #!/bin/bash
  > 
  > echo "This is only a test."
  > EOF

  $ chmod +x test.sh

# Somehow, the command below works on nix in CI (but nowhere else?!)

  $ (dune build ./message.txt &> /dev/null) || true
