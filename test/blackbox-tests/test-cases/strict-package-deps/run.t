In strict package deps mode, dependencies between packages are checked against
the package dependencies inferred by dune:
  $ cat >dune-project <<EOF
  > (lang dune 2.3)
  > (strict_package_deps)
  > (package (name bar))
  > (package (name foo))
  > EOF
  $ mkdir foo
  $ mkdir bar
  $ touch foo/foo.ml
  $ touch bar/bar.ml
  $ cat >foo/dune <<EOF
  > (executable (public_name foo) (libraries bar) (package foo))
  > EOF
  $ cat >bar/dune <<EOF
  > (library (public_name bar))
  > EOF
  $ dune build @install
  Error: Package foo is missing the following package dependencies
  - bar
  [1]

  $ cat >foo/dune <<EOF
  > (library (public_name foo) (libraries bar))
  > EOF
  $ dune build @install
  Error: Package foo is missing the following package dependencies
  - bar
  [1]
