Sources are traversed recursively (unlike pins)

  $ . ../helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkdir _foo
  $ cat >_foo/dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "file://$PWD/_bar")
  >  (package (name bar)))
  > (package
  >  (name foo)
  >  (depends bar))
  > EOF

  $ mkdir _bar
  $ cat >_bar/dune-project <<EOF
  > (lang dune 3.13)
  > (package (name bar))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "file://$PWD/_foo")
  >  (package (name foo)))
  > (package
  >  (name main)
  >  (depends foo))
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  - bar.dev
  - foo.dev
