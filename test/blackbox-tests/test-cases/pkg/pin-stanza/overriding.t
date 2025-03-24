We can override the sources set by packages we're fetching:

  $ . ../helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "file://$PWD/_bar")
  >  (package (name bar)))
  > (pin
  >  (url "file://$PWD/_foo")
  >  (package (name foo)))
  > (package
  >  (name main)
  >  (depends foo))
  > EOF

  $ mkdir _foo
  $ cat >_foo/dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "file://$PWD/_fake_bar") ;; doesn't exist but unused
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

  $ dune pkg lock
  Solution for dune.lock:
  - bar.dev
  - foo.dev
