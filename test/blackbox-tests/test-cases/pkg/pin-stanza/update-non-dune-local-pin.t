This demonstrates an issue when pinning a non-opam package. After attempting to
build the package, changes to the package are only picked up by dune after
running `dune clean`, even if building the package failed.

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
  Solution for dune.lock:
  - foo.dev

Attempt to build the packgage the first time:
(the error from make is grep'd out because it is not consistant across different systems)
  $ dune build 2>&1 | grep -v make
  echo aaa
  aaa
  false
  -> required by _build/_private/default/.pkg/foo/target/cookie

Update the message that gets printed while building foo:
  $ cat >foo/Makefile <<EOF
  > all:
  > 	echo bbb
  > 	false
  > EOF

The change to the package is ignored:
  $ dune build 2>&1 | grep -v make
  echo aaa
  aaa
  false
  -> required by _build/_private/default/.pkg/foo/target/cookie

  $ dune clean

The change to the package is picked up now that we've run `dune clean`:
  $ dune build 2>&1 | grep -v make
  echo bbb
  bbb
  false
  -> required by _build/_private/default/.pkg/foo/target/cookie
