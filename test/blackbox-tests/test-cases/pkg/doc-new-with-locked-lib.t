Regression test for https://github.com/ocaml/dune/issues/15290.

The lock-dir package below installs a library via a META file only,
without a dune-package. @doc-new therefore handles it through the
fallback odoc rules.

Before the fix, those rules tried to list the generated cmti directory
as if it were outside the build tree and crashed in
Path.as_outside_build_dir_exn. The build below now reaches odoc instead;
the expected warnings come from odoc indexing the empty fake library.

  $ mkdir external_sources

  $ cat >external_sources/META <<EOF
  > version = "0.0.1"
  > description = ""
  > archive(byte) = "fakefmt.cma"
  > EOF

  $ touch external_sources/fakefmt.cma

  $ cat >external_sources/fakefmt.install <<EOF
  > lib: [
  >  "META"
  >  "fakefmt.cma"
  > ]
  > EOF

  $ make_dune_project 3.25

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (pkg enabled)
  > EOF

  $ make_lockdir
  $ make_lockpkg fakefmt <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/external_sources))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.25)
  > (package (name foo) (depends fakefmt))
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (public_name foo)
  >  (libraries fakefmt))
  > EOF

  $ touch foo.ml

  $ dune build @doc-new
  File "fakefmt.mld", line 7, characters 0-11:
  Warning: '{!modules ...}' should not be empty.
  File "page-fakefmt.odoc":
  Warning: Failed to lookup child page dummy
