The "x" alias depends on "data" but attempts to append to it. The write fails
because dependencies are read-only, even with Dune language version 1.0.
The action still succeeds because its final touch command succeeds.

Normalize the shell-specific prefix of the error message.

  $ build () {
  >   dune build @x 2>&1 | sed 's/^.*data: Permission denied$/data: Permission denied/'
  > }
  $ echo hello > data
  $ build
  data: Permission denied
  $ cat _build/default/data
  hello

  $ build
  data: Permission denied
  $ cat _build/default/data
  hello

  $ build
  data: Permission denied
  $ cat _build/default/data
  hello
