  $ . ./helpers.sh

Generate a mock opam repository
  $ mkdir -p mock-opam-repository
  $ cat >mock-opam-repository/repo <<EOF
  > opam-version: "2.0"
  > EOF

Make a package with an extra-source field
  $ mkpkg with-extra-source <<EOF
  > opam-version: "2.0"
  > extra-source "some/file" {
  >   src: "https://some-url"
  >   checksum: "sha256=8beda92f97cde6d4a55a836ca6dc9f860bb5f1a6b765b80be4594943288571cf"
  > }
  > EOF

  $ mkdir -p mock-opam-repository/packages/with-extra-source/with-extra-source.0.0.1

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends with-extra-source))
  > EOF
  Solution for dune.lock:
  with-extra-source.0.0.1
  
  $ cat >>dune.lock/with-extra-source.pkg <<EOF
  > (source (copy $PWD/source))
  > EOF

The lockfile should contain the fetching of extra sources. It currently does not. 

  $ cat dune.lock/with-extra-source.pkg 
  (version 0.0.1)
  (source (copy $TESTCASE_ROOT/source))
