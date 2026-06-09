"Hello" is an instrumentation backend that instruments by printing "Hello,
Dune!" at the beginning of the module.

  $ make_instrumentation_backends
  $ make_basic_instrumentation_project

As instrumentation is disabled, this should not print the instrumentation
message.

  $ dune build
  $ _build/default/main.exe

This should print the instrumentation message twice, once for "main" and once
for "mylib":

  $ dune build --instrument-with hello
  $ _build/default/main.exe
  Hello from Mylib!
  Hello from Main!

An empty file:

  $ cat >main.ml <<EOF
  > EOF

We build the empty file.

  $ dune build

Nothing happens:

  $ _build/default/main.exe

We rebuild with instrumentation via the CLI.

  $ dune build --instrument-with hello

We get the message.

  $ _build/default/main.exe
  Hello from Main!
