%{pkg:foo:section} expands to a directory path. The path is always computed
from the install layout convention, even for sections the package doesn't
install to. This is consistent across all package types.

For installed packages, the path is derived from the install prefix:

  $ mkdir -p src prefix consumer

  $ cat >src/dune-project <<EOF
  > (lang dune 3.23)
  > (package (name extpkg))
  > EOF

  $ cat >src/dune <<EOF
  > (install (section share) (package extpkg) (files (hello.txt as hello.txt)))
  > EOF

  $ cat >src/hello.txt <<EOF
  > hello from installed
  > EOF

  $ dune build --root src 2>&1
  Entering directory 'src'
  Leaving directory 'src'

  $ dune install --root src --prefix $PWD/prefix --display short 2>&1
  Installing $TESTCASE_ROOT/prefix/lib/extpkg/META
  Installing $TESTCASE_ROOT/prefix/lib/extpkg/dune-package
  Installing $TESTCASE_ROOT/prefix/share/extpkg/hello.txt

  $ cat >consumer/dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat >consumer/dune <<EOF
  > (rule
  >  (alias test-installed-missing)
  >  (action (echo "%{pkg:extpkg:sbin}\n")))
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root consumer @test-installed-missing 2>&1
  Entering directory 'consumer'
  $TESTCASE_ROOT/prefix/sbin
  Leaving directory 'consumer'

For workspace packages, similarly returns the path even though foo only
installs to share:

  $ cat >dune-project <<EOF
  > (lang dune 3.23)
  > (package (name foo))
  > EOF

  $ mkdir -p foo

  $ cat >foo/dune <<EOF
  > (install (section share) (package foo) (files (data.txt as data.txt)))
  > EOF

  $ cat >foo/data.txt <<EOF
  > some data
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias test-local-missing)
  >  (action (echo "%{pkg:foo:sbin}\n")))
  > EOF

  $ dune build @test-local-missing 2>&1
  ../install/default/sbin
