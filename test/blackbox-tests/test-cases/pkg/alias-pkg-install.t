This test verifies the @pkg-deps alias fetch and build the project dependencies
without building the project itself.

  $ . ./helpers.sh

Create a fake library to install in the target project as a dependency:
  $ mkdir foo
  $ cd foo
  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package (name foo))
  > EOF
  $ cat > foo.ml <<EOF
  > let foo = "Hello, World!"
  > EOF
  $ cat > dune <<EOF
  > (library
  >  (public_name foo))
  > EOF
  $ cd ..
  $ tar cf foo.tar foo
  $ rm -rf foo

Make an opam package for the library:
  $ make_lockdir
  $ cat > dune.lock/foo.pkg << EOF
  > (version 0.0.1)
  > (build
  >  (run dune build -p %{pkg-self:name} @install))
  > (source
  >  (fetch (url file://$PWD/foo.tar)))
  > EOF

Create a project using the fake library as a dependency:
  $ cat > dune-project << EOF
  > (lang dune 3.16)
  > (package
  >  (name bar)
  >  (allow_empty)
  >  (depends foo))
  > EOF

The alias call builds the `foo` dependency but not the project itself. We
verify we have the `foo` package but not the `bar.exe` in the build directory:
  $ dune build @pkg-install
  $ ls _build/_private/default/.pkg/
  foo
  $ ls _build/default/
