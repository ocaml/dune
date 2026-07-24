This demonstrates an issue when pinning a non-opam package. Changes made to the
pinned package's source are visible in the current project _unless_ the pinned
package's opam file is changed, in which case it's necessary to run `dune pkg
lock`. This is because the pinned package is built using build commands copied
to the package's lockfile in the current project, and they are only re-copied
when running `dune pkg lock`. This is inconsistent with how dune treats other
files in the pinned package.

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
  >  [ make "foo" ]
  > ]
  > EOF
  $ cat >foo/Makefile <<EOF
  > foo:
  > 	echo aaa
  > 	false
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  - foo.dev

Attempt to build the packgae the first time:
  $ dune build
  echo aaa
  aaa
  false
  make: *** [foo] Error 1
  -> required by _build/_private/default/.pkg/foo/target/cookie
  [1]

Change the message printed while building the package to demonstrate that it
gets picked up by dune:
  $ cat >foo/Makefile <<EOF
  > foo:
  > 	echo bbb
  > 	false
  > EOF

Rebuild the packgae to see the new message. Note that running `dune clean` is
currently necessary for this to work (see
https://github.com/ocaml/dune/issues/10232).
  $ dune clean
  $ dune build
  echo bbb
  bbb
  false
  make: *** [foo] Error 1
  -> required by _build/_private/default/.pkg/foo/target/cookie
  [1]

Now change the pinned package again, this time changing the name of the
makefile target being built. Note that this requires changing the build command
in the opam file to use the new target.
  $ cat >foo/foo.opam <<EOF
  > opam-version: "2.0"
  > build: [
  >  [ make "bar" ]
  > ]
  > EOF
  $ cat >foo/Makefile <<EOF
  > bar:
  > 	echo bbb
  > 	false
  > EOF

But dune still tries to build the old makefile target.
  $ dune clean
  $ dune build
  make: *** No rule to make target `foo'.  Stop.
  -> required by _build/_private/default/.pkg/foo/target/cookie
  [1]

For dune to see the change requires rerunning `dune pkg lock` which will copy
the build commands from the opam file into the lockdir.
  $ dune pkg lock
  Solution for dune.lock:
  - foo.dev
  $ dune clean
  $ dune build
  echo bbb
  bbb
  false
  make: *** [bar] Error 1
  -> required by _build/_private/default/.pkg/foo/target/cookie
  [1]
