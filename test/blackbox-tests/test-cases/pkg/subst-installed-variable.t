Test the %{pkg:installed}% form inside file substitution:

  $ . ./helpers.sh

  $ make_lockdir
  $ mkdir source
  $ cat >source/foo.in <<EOF
  > foo: %{somepkg:installed}%
  > EOF
  $ cat >dune.lock/test.pkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/source))
  > (build
  >  (progn
  >   (system "echo somepkg installation %{pkg:somepkg:installed}")
  >   (substitute foo.in foo)
  >   (system "cat foo")))
  > EOF

  $ build_pkg test
  somepkg installation false
  foo: false
