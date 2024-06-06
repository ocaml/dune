Virtual library implementations are not correctly recorded when pulled through
other implementations.

Reproduces the issue reported in #10460

  $ cat >dune-project <<EOF
  > (lang dune 3.15)
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries impl2 impl1))
  > EOF
  $ touch main.ml

  $ mkdir vlib1 vlib2 impl1 impl2 lib

  $ cat >vlib1/dune <<EOF
  > (library
  >  (name vlib1)
  >  (virtual_modules vlib1))
  > EOF
  $ touch vlib1/vlib1.mli

  $ cat >impl1/dune <<EOF
  > (library
  >  (name impl1)
  >  (implements vlib1))
  > EOF
  $ touch impl1/vlib1.ml

  $ cat >vlib2/dune <<EOF
  > (library
  >  (name vlib2)
  >  (virtual_modules vlib2))
  > EOF
  $ touch vlib2/vlib2.mli

  $ cat >impl2/dune <<EOF
  > (library
  >  (name impl2)
  >  (implements vlib2)
  >  (libraries lib))
  > EOF
  $ touch impl2/vlib2.ml

  $ cat >lib/dune <<EOF
  > (library
  >  (name lib)
  >  (libraries vlib1))
  > EOF

  $ dune build main.exe
