"Hello" is an instrumentation backend that instruments by printing "Hello,
Dune!" at the beginning of the module.

  $ make_instrumentation_backends
  $ make_basic_instrumentation_project
  $ exe=./main.exe
  $ built_exe=_build/default/main.exe

As instrumentation is disabled, this should not print the instrumentation
message.

  $ dune build "$exe"
  $ "$built_exe"

This should print the instrumentation message twice, once for "main" and once
for "mylib":

  $ dune build --instrument-with hello "$exe"
  $ "$built_exe"
  Hello from Mylib!
  Hello from Main!

An empty file:

  $ cat >main.ml <<EOF
  > EOF

We build the empty file.

  $ dune build "$exe"

Nothing happens:

  $ "$built_exe"

We rebuild with instrumentation via the CLI.

  $ dune build --instrument-with hello "$exe"

We get the message.

  $ "$built_exe"
  Hello from Main!
