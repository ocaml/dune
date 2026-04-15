Test that %{pkg:...} supports cross-package references in regular rules.

  $ cat >dune-project <<EOF
  > (lang dune 3.23)
  > (package (name foo))
  > (package (name bar))
  > EOF

  $ mkdir foo bar

  $ cat >foo/dune <<EOF
  > (install
  >  (section share)
  >  (package foo)
  >  (files (data.txt as data.txt)))
  > EOF

  $ cat >foo/data.txt <<EOF
  > foo data
  > EOF

  $ cat >bar/dune <<EOF
  > (install
  >  (section share)
  >  (package bar)
  >  (files (greeting.txt as greeting.txt)))
  > EOF

  $ cat >bar/greeting.txt <<EOF
  > hello
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias test-cross-pkg)
  >  (action
  >   (echo
  >    "\| foo-share: %{pkg:foo:share}
  >    "\| bar-share: %{pkg:bar:share}
  >   )))
  > EOF

  $ dune build @test-cross-pkg 2>&1
  foo-share: ../install/default/share/foo
  bar-share: ../install/default/share/bar
