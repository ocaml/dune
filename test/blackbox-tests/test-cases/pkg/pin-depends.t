Demonstrate our support for pin-depends.

  $ . ./helpers.sh

  $ add_mock_repo_if_needed
  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF
  $ mkrepo
  $ mkpkg bar 0.0.1

  $ runtest() {
  > cat >foo.opam <<EOF
  > opam-version: "2.0"
  > depends: [ "bar" ]
  > pin-depends: [ "bar.1.0.0" "$1" ]
  > EOF
  > dune pkg lock
  > cat dune.lock/bar.pkg
  > }

Local pinned source:

  $ mkdir _bar_file
  $ cat >_bar_file/opam <<EOF
  > opam-version: "2.0"
  > EOF
  $ runtest "file://$PWD/_bar_file"
  Solution for dune.lock:
  - bar.0.0.1
  (version 0.0.1)

Git pinned source:

  $ mkdir _bar_git
  $ cd _bar_git
  $ git init --quiet
  $ cat >opam <<EOF
  > opam-version: "2.0"
  > EOF
  $ git add -A
  $ git commit --quiet -m "Initial commit"
  $ cd ..
  $ runtest "git+file://$PWD/_bar_git"
  Solution for dune.lock:
  - bar.0.0.1
  (version 0.0.1)
