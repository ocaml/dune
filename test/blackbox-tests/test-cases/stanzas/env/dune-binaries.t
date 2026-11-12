Adds custom binaries through the `env` stanza.

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

Once the binary has been built, a sandboxed rule can use it without declaring a
dependency. It is absent from the sandbox but still reachable through PATH in
the real build directory. This leaks an undeclared input into the action.

  $ cat >> dune <<'EOF'
  > (rule
  >  (target sandboxed.txt)
  >  (deps (sandbox always))
  >  (action
  >   (with-stdout-to %{target}
  >    (system
  >     "pwd && test ! -e .bin/foobar && command -v foobar && foobar"))))
  > EOF
  $ dune build sandboxed.txt
  $ cat _build/default/sandboxed.txt | censor
  $PWD/_build/.sandbox/$DIGEST/default
  $PWD/_build/default/.bin/foobar
  This is only a test.
