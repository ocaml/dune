We try to pull an opam package that isn't a dune project

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
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  - foo.dev
  $ pkg="dune.lock/foo.pkg"
  $ grep version $pkg
  (version dev)
  $ grep dev $pkg
  (version dev)
  (dev)
  $ grep "file://" $pkg | sed "s#$PWD#PWD#g"
     file://PWD/_foo)))
