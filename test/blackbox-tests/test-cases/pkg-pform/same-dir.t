Test that %{pkg:foo:section:file} works from a rule inside the package
directory itself.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name foo) (dir foo))
  > EOF

  $ mkdir foo

  $ cat >foo/dune <<EOF
  > (install
  >  (section share)
  >  (package foo)
  >  (files (src.txt as dest.txt)))
  > (rule
  >  (alias test-self-ref)
  >  (action (cat %{pkg:foo:share:dest.txt})))
  > EOF

  $ cat >foo/src.txt <<EOF
  > some data
  > EOF

  $ dune build @test-self-ref 2>&1
  some data
