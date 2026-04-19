Test that %{pkg:foo:section:file} resolves to the source file for workspace
packages.

  $ cat >dune-project <<EOF
  > (lang dune 3.23)
  > (package (name foo))
  > EOF

  $ mkdir foo

  $ cat >foo/dune <<EOF
  > (install
  >  (section share)
  >  (package foo)
  >  (files (src.txt as dest.txt)))
  > EOF

  $ cat >foo/src.txt <<EOF
  > some data
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias test-pkg-file)
  >  (action (echo "%{pkg:foo:share:dest.txt}\n")))
  > EOF

  $ dune build @test-pkg-file 2>&1
  foo/src.txt
