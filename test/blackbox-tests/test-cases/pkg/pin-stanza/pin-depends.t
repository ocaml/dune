Setting the source of a package to a non dune package with pin-depends should
respect the pin-depends

  $ . ../helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "file://$PWD/_foo")
  >  (package (name foo)))
  > (package
  >  (name main)
  >  (depends foo))
  > EOF

  $ mkdir _foo
  $ cat >_foo/foo.opam <<EOF
  > opam-version: "2.0"
  > build: [ "echo" "foo" ]
  > depends: [ "bar" ]
  > pin-depends: [ "bar.1.0.0" "file://$PWD/_bar" ]
  > EOF

  $ mkdir _bar
  $ cat >_bar/bar.opam <<EOF
  > opam-version: "2.0"
  > build: [ "echo" "bar" ]
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  - bar.1.0.0
  - foo.dev
