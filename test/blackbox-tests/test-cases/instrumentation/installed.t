Check that the backend can be used when it is installed.

  $ make_instrumentation_backends

  $ dune build ppx/hello.install
  $ dune install hello --prefix _install
  $ grep instrumentation.backend _install/lib/hello/dune-package
   (instrumentation.backend hello.ppx))
  $ mkdir -p installed
  $ cat >installed/dune-workspace <<EOF
  > (lang dune 2.7)
  > (instrument_with hello)
  > EOF
  $ cat >installed/dune-project <<EOF
  > (lang dune 2.7)
  > (wrapped_executables false)
  > EOF
  $ cat >installed/dune <<EOF
  > (executable
  >  (name main)
  >  (instrumentation (backend hello)))
  > EOF
  $ cat >installed/main.ml <<EOF
  > EOF
  $ OCAMLPATH=$PWD/_install/lib:$OCAMLPATH dune build --root installed
  $ installed/_build/default/main.exe
  Hello from Main!
