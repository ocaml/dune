Make sure that `dune install` does not install vendored packages.

In the following example, the main package is "foo" and "bar" is a
vendored package that shouldn't be installed.

  $ mkdir -p vendor/bar
  $ touch file
  $ touch vendor/bar/file

  $ cat >dune-project <<EOF
  > (lang dune 2.1)
  > (package (name foo))
  > EOF

  $ cat >dune <<EOF
  > (install (section lib) (files file))
  > (vendored_dirs vendor)
  > EOF

  $ cat >vendor/bar/dune-project <<EOF
  > (lang dune 2.1)
  > (package (name bar))
  > EOF

  $ cat >vendor/bar/dune <<EOF
  > (install (section lib) (files file))
  > EOF

  $ dune build @install

The following command should produce no output:

  $ dune install --prefix _install --display short 2>&1 | grep bar
  [1]
