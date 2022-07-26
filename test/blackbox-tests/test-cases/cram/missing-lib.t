  $ mkdir -p lib test/uselib.t

  $ cat >dune-project<<EOF
  > (lang dune 3.4)
  > EOF

  $ touch hello-world.opam

  $ cat >lib/dune<<EOF
  > (library
  >  (name hello_world)
  >  (public_name hello-world)
  >  (wrapped false))
  > EOF

  $ cat >lib/hello_world.ml<<EOF
  > let hello_world () = Printf.printf "Hello, World!\n%!"
  > EOF

  $ cat >test/dune<<EOF
  > ;(cram
  > ; (deps
  > ;  (package hello-world)))
  > EOF

  $ cat >test/uselib.t/dune<<EOF
  > (executable
  >  (name hello)
  >  (libraries hello-world))
  > EOF

  $ cat >test/uselib.t/dune-project<<EOF
  > (lang dune 3.4)
  > EOF

  $ cat >test/uselib.t/hello.ml<<EOF
  > let () = Hello_world.hello_world ()
  > EOF

  $ cat >test/uselib.t/run.t<<EOF
  >   $ dune build
  >   $ ./_build/default/hello.exe
  >   Hello, World!
  > EOF

The following should fail at the second command.

  $ dune build
  $ dune build @uselib

The following fails as expected at the second command.

  $ dune clean
  $ dune build @uselib
  File "test/uselib.t/run.t", line 1, characters 0-0:
  Error: Files _build/default/test/uselib.t/run.t and
  _build/default/test/uselib.t/run.t.corrected differ.
  [1]
