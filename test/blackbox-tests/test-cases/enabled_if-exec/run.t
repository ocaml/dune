Test that `enabled_if` fields work as expected for executables.
Since 2.3.

This executable is disabled, any attempt to build it should fail:
  $ dune build dis.exe
  Error: Don't know how to build dis.exe
  [1]
  $ dune exec ./dis.exe
  Error: Program "./dis.exe" not found!
  [1]

This one is enabled
  $ dune exec ./main.exe
  Pong

Installing should silently ignore disabled executables
  $ dune build @install
