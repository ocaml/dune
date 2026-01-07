Shows what happens when Dune tries to run an executable script on all our
supported platforms.

  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat > dune << EOF
  > (rule (with-stdout-to output (run ./executable.sh)))
  > EOF

  $ cat >executable.sh <<EOF
  > #!/bin/sh
  > echo "Executable ran successfully!"
  > EOF
  $ chmod +x executable.sh

  $ dune build --root . @all
  $ cat _build/default/output
  Executable ran successfully!

  $ cat >executable.sh <<EOF
  > #!/usr/bin/env sh
  > echo "Executable ran successfully using sh!"
  > EOF
  $ chmod +x executable.sh

  $ dune build --root . @all
  $ cat _build/default/output
  Executable ran successfully using sh!

  $ cat >executable.sh <<EOF
  > #!/usr/bin/env -S sh -v
  > echo "Executable ran successfully using sh -v!"
  > EOF
  $ chmod +x executable.sh

  $ dune build --root . @all
  #!/usr/bin/env -S sh -v
  echo "Executable ran successfully using sh -v!"
  $ cat _build/default/output
  Executable ran successfully using sh -v!
