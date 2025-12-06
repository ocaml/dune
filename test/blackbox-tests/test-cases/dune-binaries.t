  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat >dune <<EOF
  > (env
  >  (_
  >   (binaries (test.sh as test))
  >   (flags :standard)))
  > 
  > (rule
  >  (target message.txt)
  >  (action (with-stdout-to %{target} (run test))))
  > EOF

  $ cat >test.sh <<EOF
  > #!/usr/bin/env bash
  > 
  > echo "This is only a test."
  > EOF

  $ chmod +x test.sh
  $ dune build
  Error: execve(.bin/test): No such file or directory
  -> required by _build/default/message.txt
  -> required by alias all
  -> required by alias default
  [1]
