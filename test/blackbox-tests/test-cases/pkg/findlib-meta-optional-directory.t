Demonstrate the handling of findlib directories that don't exist

Reproduces #11405

  $ . ./helpers.sh

  $ mkdir external_sources

  $ cat >external_sources/META <<EOF
  > package "sub" (
  >   directory = "sub"
  >   version = "0.0.1"
  >   exists_if = "sub.cma"
  > )
  > EOF

  $ cat >external_sources/mypkg.install <<EOF
  > lib: [
  >  "META"
  > ]
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > EOF

  $ mkdir dune.lock

  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF

  $ cat >dune.lock/mypkg.pkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/external_sources))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (libraries mypkg.sub)
  >  (name foo))
  > EOF

  $ dune build foo.exe
  Error: This rule defines a directory target "default/.pkg/mypkg/target" that
  matches the requested path "default/.pkg/mypkg/target/lib/mypkg/sub" but the
  rule's action didn't produce it
  [1]
