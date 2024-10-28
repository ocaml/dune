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
  $ mkpkg foo <<EOF
  > build: [
  >   [
  >     "dune"
  >     "build"
  >     "-p"
  >     name
  >     "@install"
  >   ]
  > ]
  > url {
  >  src: "$PWD/foo.tar"
  > }
  > EOF

Create a project using the fake library as a dependency:
  $ cat > dune-project << EOF
  > (lang dune 3.16)
  > (package
  >  (name bar)
  >  (allow_empty)
  >  (depends foo))
  > EOF
  $ add_mock_repo_if_needed
  $ dune pkg lock
  Solution for dune.lock:
  - foo.0.0.1

Create an executable which uses the fake dependency. It ensures we build the
dependency package but not the package:
  $ cat > dune << EOF
  > (executable
  >  (name bar)
  >  (libraries foo))
  > EOF
  $ cat > bar.ml << EOF
  > let () = print_endline "source"
  > EOF

The alias call builds the `foo` dependency but not the project itself. We
verify we have the `foo` package but not the `bar.exe` in the build directory:
  $ dune build @pkg-deps
  $ ls _build/_private/default/.pkg/
  foo
  $ ls _build/default/

Verify that building the `bar.exe` file is working:
  $ dune build ./bar.exe
  $ ls _build/default/bar.exe
  _build/default/bar.exe
