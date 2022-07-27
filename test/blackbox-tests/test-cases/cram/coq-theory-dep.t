  $ mkdir -p theory test/usetheory.t

  $ cat >dune-project<<EOF
  > (lang dune 3.4)
  > (using coq 0.5)
  > EOF

  $ touch hello-world.opam

  $ cat >theory/dune<<EOF
  > (coq.theory
  >  (name hello.world)
  >  (package hello-world))
  > EOF

  $ cat >theory/hello_world.v<<EOF
  > From Coq.Strings Require Import String.
  > 
  > Definition hello_world : string := "Hello, World!".
  > EOF

  $ cat >test/dune<<EOF
  > (cram
  >  (deps
  >   (package hello-world)))
  > EOF

  $ cat >test/usetheory.t/dune<<EOF
  > (coq.theory
  >  (name hello)
  >  (theories hello.world))
  > EOF

  $ cat >test/usetheory.t/dune-project<<EOF
  > (lang dune 3.4)
  > (using coq 0.5)
  > EOF

  $ cat >test/usetheory.t/hello.v<<EOF
  > From hello.world Require Import hello_world.
  > 
  > Eval simpl in hello_world.
  > EOF

  $ cat >test/usetheory.t/run.t<<EOF
  >   $ dune build
  >   XXX
  > EOF

The following should succeed.

  $ dune build @usetheory
  File "test/usetheory.t/run.t", line 1, characters 0-0:
  Error: Files _build/default/test/usetheory.t/run.t and
  _build/default/test/usetheory.t/run.t.corrected differ.
  [1]
