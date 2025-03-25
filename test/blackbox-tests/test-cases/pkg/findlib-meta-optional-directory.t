Demonstrate the handling of findlib directories that don't exist

Reproduces #11405

  $ . ./helpers.sh

  $ mkdir external_sources

  $ cat >external_sources/META <<EOF
  > package "yes" (
  >   directory = "yes"
  >   version = "0.0.1"
  >   exists_if = "yes.cma"
  > )
  > package "no" (
  >   directory = "no"
  >   version = "0.0.1"
  >   exists_if = "no.cma"
  > )
  > EOF

  $ cat >external_sources/mypkg.install <<EOF
  > lib: [
  >  "META"
  >  "yes/yes.cma" {"yes/yes.cma"}
  > ]
  > EOF

  $ mkdir external_sources/yes
  $ touch external_sources/yes/yes.cma

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

  $ touch foo.ml

  $ cat >dune <<EOF
  > (executable
  >  (libraries mypkg.yes)
  >  (name foo))
  > EOF

  $ dune build foo.exe
  Error: This rule defines a directory target "default/.pkg/mypkg/target" that
  matches the requested path "default/.pkg/mypkg/target/lib/mypkg/no" but the
  rule's action didn't produce it
  [1]

No difference between below and above even though 'yes' actually exists.

  $ cat >dune <<EOF
  > (executable
  >  (libraries mypkg.no)
  >  (name foo))
  > EOF

  $ dune build foo.exe
  Error: This rule defines a directory target "default/.pkg/mypkg/target" that
  matches the requested path "default/.pkg/mypkg/target/lib/mypkg/no" but the
  rule's action didn't produce it
  [1]
