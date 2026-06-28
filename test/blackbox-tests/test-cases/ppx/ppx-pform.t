Test the %{ppx:...} pform that creates a combined ppx executable

  $ make_dune_project 3.21

Create two simple ppx rewriters

  $ mkdir ppx1 ppx2

  $ cat >ppx1/dune <<EOF
  > (library
  >  (name ppx1)
  >  (kind ppx_rewriter))
  > EOF

  $ cat >ppx1/ppx1.ml <<EOF
  > let () = Ppxlib.Driver.register_transformation "ppx1"
  >   ~impl:(fun structure -> structure)
  > EOF

  $ cat >ppx2/dune <<EOF
  > (library
  >  (name ppx2)
  >  (kind ppx_rewriter))
  > EOF

  $ cat >ppx2/ppx2.ml <<EOF
  > let () = Ppxlib.Driver.register_transformation "ppx2"
  >   ~impl:(fun structure -> structure)
  > EOF

Create a rule that uses the ppx pform

  $ cat >dune <<EOF
  > (rule
  >  (alias test-ppx)
  >  (action (system "echo %{ppx:ppx1+ppx2}")))
  > EOF

Run the test

  $ dune build @test-ppx 2>&1 | censor
  .ppx/$DIGEST/ppx.exe

Force the generated ppx executable to be built:

  $ cat >dune <<EOF
  > (rule
  >  (alias test-ppx-run)
  >  (action
  >   (with-stdout-to ppx-help
  >    (run %{ppx:ppx1+ppx2} --help))))
  > EOF

  $ dune build @test-ppx-run 2>&1 | censor
  Error: I don't know what ppx rewriters set $DIGEST
  correspond to.
  -> required by _build/default/ppx-help
  -> required by alias test-ppx-run in dune:1
  [1]

Test that the order of libraries doesn't matter

  $ cat >dune <<EOF
  > (rule
  >  (alias test-ppx)
  >  (action (system "echo %{ppx:ppx2+ppx1}")))
  > EOF

  $ dune build @test-ppx


Invalid ppx form

  $ cat >dune <<EOF
  > (rule
  >  (alias test-ppx)
  >  (action (system "echo %{ppx:.faz+bar}")))
  > EOF

  $ dune build @test-ppx
  File "dune", line 3, characters 23-38:
  3 |  (action (system "echo %{ppx:.faz+bar}")))
                             ^^^^^^^^^^^^^^^
  Error: ".faz" is an invalid library name.
  [1]
