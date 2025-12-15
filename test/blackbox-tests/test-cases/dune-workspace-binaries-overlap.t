Demonstrate a failure of dune to resolve a binary added via binaries in
a dune-workspace file:

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.21)
  > 
  > (env
  >  (_
  >   (binaries (workspace.sh as foobar))
  >   (flags :standard)))
  > EOF

  $ cat >dune <<EOF
  > (env
  >  (_
  >   (binaries (dune-file.sh as foobar))))
  > (rule
  >  (target message.txt)
  >  (action (with-stdout-to %{target} (run foobar))))
  > EOF

  $ cat >workspace.sh <<EOF
  > #!/bin/sh
  > echo "Workspace."
  > EOF
  $ chmod +x workspace.sh

  $ cat >dune-file.sh <<EOF
  > #!/bin/sh
  > echo "Dune file."
  > EOF
  $ chmod +x dune-file.sh

  $ cat >context.sh <<EOF
  > #!/bin/sh
  > echo "Context."
  > EOF
  $ chmod +x context.sh

Dune file binaries override the workspace binaries. Expecting "Dune file."

  $ dune build ./message.txt
  $ cat _build/default/message.txt
  Dune file.

  $ cat >dune-workspace <<EOF
  > (lang dune 3.21)
  > 
  > (context
  >  (default
  >   (name default)))
  > (context
  >  (default
  >   (name other_context)
  >   (env
  >    (_
  >     (binaries (context.sh as foobar))
  >     (flags :standard)))))
  > (env
  >  (_
  >   (binaries (workspace.sh as foobar))
  >   (flags :standard)))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (target message.txt)
  >  (action (with-stdout-to %{target} (run foobar))))
  > EOF

Workspace binary should print "Workspace."

  $ dune clean
  $ dune build ./message.txt
  File "dune", lines 1-3, characters 0-78:
  1 | (rule
  2 |  (target message.txt)
  3 |  (action (with-stdout-to %{target} (run foobar))))
  Error: No rule found for .bin/foobar
  File "dune", lines 1-3, characters 0-78:
  1 | (rule
  2 |  (target message.txt)
  3 |  (action (with-stdout-to %{target} (run foobar))))
  Error: No rule found for .bin/foobar (context other_context)
  [1]
  $ cat _build/default/message.txt
  cat: _build/default/message.txt: No such file or directory
  [1]

Context binaries override the workspace binaries. Expecting "Context."

  $ cat _build/other_context/message.txt
  cat: _build/other_context/message.txt: No such file or directory
  [1]

