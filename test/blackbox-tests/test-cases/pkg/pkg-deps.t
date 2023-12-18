We should be able to specify (package ..) deps on locally built packages.

  $ . ./helpers.sh

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > EOF

  $ make_lockdir
  $ cat >dune.lock/foo.pkg <<EOF
  > (version 0.0.1)
  > (install
  >  (progn
  >   (run mkdir -p %{prefix}/bin)
  >   (run touch %{prefix}/bin/foo)
  >   (run chmod +x %{prefix}/bin/foo)))
  > EOF

  $ cat >dune <<'EOF'
  > (dirs :standard \ external_sources)
  > (rule
  >  (alias foo)
  >  (action
  >   (progn
  >    (run which foo)
  >    (echo %{bin:foo})))
  >  (deps (package foo)))
  > EOF

  $ dune build @foo
  $TESTCASE_ROOT/_build/_private/default/.pkg/foo/target/bin/foo
  ../_private/default/.pkg/foo/target/bin/foo

Now we define the external package using a dune project:

  $ mkdir external_sources
  $ cat >external_sources/dune-project <<EOF
  > (lang dune 3.11)
  > (package (name foo))
  > EOF
  $ cat >external_sources/dune <<EOF
  > (executable
  >  (public_name foo))
  > EOF
  $ cat >external_sources/foo.ml <<EOF
  > print_endline "Hello from foo.ml!"
  > EOF

  $ cat >dune.lock/foo.pkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/external_sources))
  > (build (run dune build @install --promote-install-files))
  > EOF
  $ dune build @foo
  $TESTCASE_ROOT/_build/_private/default/.pkg/foo/target/bin/foo
  ../_private/default/.pkg/foo/target/bin/foo
