This demonstrates pinning a non-opam package and then modifying its sources.
Whenever the sources are modified, dune should rebuild the package in the
workspace where it's locked.

  $ . ../helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "file://$PWD/foo")
  >  (package (name foo)))
  > (package
  >  (name main)
  >  (depends foo))
  > EOF

Make a package "foo" whose build will fail after printing a message:
  $ mkdir foo
  $ cat >foo/foo.opam <<EOF
  > opam-version: "2.0"
  > build: [
  >  [ make ]
  > ]
  > EOF
  $ cat >foo/Makefile <<EOF
  > all:
  > 	echo aaa
  > 	false
  > EOF

  $ dune pkg lock
  Solution for .dune-solution-cache:
  - foo.dev

Attempt to build the package the first time:
(the error from make is grep'd out because it is not consistant across different systems)
  $ build_pkg foo 2>&1 | grep -v -e "^make" -e "^gmake"
  echo aaa
  aaa
  false
  File ".dune-solution-cache/foo.pkg", line 4, characters 6-13:
  4 |  (run %{make}))
            ^^^^^^^
  Error: Logs for package foo
  

Update the message that gets printed while building foo:
  $ cat >foo/Makefile <<EOF
  > all:
  > 	echo bbb
  > 	false
  > EOF

The change to the package is picked up:
  $ build_pkg foo 2>&1 | grep -v -e "^make" -e "^gmake"
  echo bbb
  bbb
  false
  File ".dune-solution-cache/foo.pkg", line 4, characters 6-13:
  4 |  (run %{make}))
            ^^^^^^^
  Error: Logs for package foo
  
