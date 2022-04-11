In strict package deps mode, dependencies between packages are checked against
the package dependencies inferred by dune:
  $ mkdir simple && cd simple
  $ cat >dune-project <<EOF
  > (lang dune 2.3)
  > (strict_package_deps)
  > (package (name bar))
  > (package (name foo))
  > EOF
  $ mkdir foo bar
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
  -> required by _build/default/foo.install
  -> required by alias install
  [1]

  $ cat >foo/dune <<EOF
  > (library (public_name foo) (libraries bar))
  > EOF
  $ dune build @install
  Error: Package foo is missing the following package dependencies
  - bar
  -> required by _build/default/foo.install
  -> required by alias install
  [1]
  $ cd ..

Strict package deps reports missing dependencies because it does not check
transitive deps.

  $ mkdir transitive && cd transitive
  $ cat >dune-project <<EOF
  > (lang dune 2.3)
  > (strict_package_deps)
  > (package (name baz))
  > (package (name bar) (depends baz))
  > (package (name foo) (depends bar))
  > EOF
  $ touch baz.ml bar.ml foo.ml
  $ cat >dune <<EOF
  > (library (public_name baz) (modules baz))
  > (library (public_name bar) (libraries baz) (modules bar))
  > (library (public_name foo) (libraries bar) (modules foo))
  > EOF
  $ dune build @install
  Error: Package foo is missing the following package dependencies
  - baz
  -> required by _build/default/foo.install
  -> required by alias install
  [1]
