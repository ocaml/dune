  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > EOF

"Hello" is an instrumentation backend that instruments by printing "Hello,
Dune!" at the beginning of the module.

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries mylib)
  >  (instrumentation (backend hello)))
  > 
  > (library
  >  (name mylib)
  >  (modules mylib)
  >  (instrumentation (backend hello)))
  > EOF

  $ cat >mylib.ml <<EOF
  > let f () = print_endline "Mylib"
  > EOF

  $ cat >main.ml <<EOF
  > let () = Mylib.f ()
  > EOF

As instrumentation is disabled, this should not print the instrumentation
message.

  $ dune build
  $ _build/default/main.exe
  Mylib

This should print the instrumentation message twice, once for "main" and once
for "mylib":

  $ dune build --instrument-with hello
  $ _build/default/main.exe
  Hello, Dune!
  Hello, Dune!
  Mylib

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
  Hello, Dune!

Can also enable with an environment variable.

  $ DUNE_INSTRUMENT_WITH=hello dune build

  $ _build/default/main.exe
  Hello, Dune!

Instrumentation can also be controlled by using the dune-workspace file.

  $ cat >dune-workspace <<EOF
  > (lang dune 2.7)
  > (instrument_with hello)
  > EOF

  $ dune build

  $ _build/default/main.exe
  Hello, Dune!

It can also be controlled on a per-context scope.

  $ cat >dune-workspace <<EOF
  > (lang dune 2.7)
  > (context (default (name coverage) (instrument_with hello)))
  > EOF

  $ dune build

  $ _build/coverage/main.exe
  Hello, Dune!

Per-context setting takes precedence over per-workspace setting.

  $ cat >dune-workspace <<EOF
  > (lang dune 2.7)
  > (instrument_with hello)
  > (context (default (name coverage) (instrument_with)))
  > EOF

  $ dune build

  $ _build/coverage/main.exe
