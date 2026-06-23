Reproduction for https://github.com/ocaml/dune/issues/15290

Building @doc-new with package management enabled crashes when a locked
package installs a library via a META file (without a dune-package), because
the odoc rules classify it as a "fallback" directory and try to read the
cmti directory as if it were outside the build tree.

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

  $ dune build @doc-new 2>&1 | grep "Internal error"
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  [1]
