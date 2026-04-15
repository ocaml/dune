Test that %{pkg:...} supports cross-package file references.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name foo))
  > (package (name bar))
  > EOF

  $ mkdir foo bar

  $ cat >foo/dune <<EOF
  > (install
  >  (section share)
  >  (package foo)
  >  (files (foo_src.txt as foo_data.txt)))
  > EOF

  $ cat >foo/foo_src.txt <<EOF
  > foo data
  > EOF

  $ cat >bar/dune <<EOF
  > (install
  >  (section share)
  >  (package bar)
  >  (files (bar_src.txt as bar_data.txt)))
  > EOF

  $ cat >bar/bar_src.txt <<EOF
  > hello
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias test-cross-pkg)
  >  (action
  >   (progn
  >    (cat %{pkg:foo:share:foo_data.txt})
  >    (cat %{pkg:bar:share:bar_data.txt}))))
  > EOF

  $ dune build @test-cross-pkg 2>&1
  foo data
  hello
