This demonstrates pinning a non-opam package and then modifying its sources.
Whenever the sources are modified, dune should rebuild the package in the
workspace where it's locked.

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
  >  [ "./make" ]
  > ]
  > EOF
  $ cat > foo/make <<EOF
  > #!/bin/sh
  > echo aaa
  > exit 1
  > EOF
  $ chmod +x foo/make

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - foo.dev

Attempt to build the package the first time:

  $ build_pkg foo
  aaa
  File "dune.lock/foo.dev.pkg", line 4, characters 30-36:
  4 |  (all_platforms ((action (run ./make)))))
                                    ^^^^^^
  Error: Logs for package foo
  
  [1]

Update the message that gets printed while building foo:
  $ cat > foo/make <<EOF
  > #!/bin/sh
  > echo bbb
  > exit 1
  > EOF

The change to the package is picked up:
  $ build_pkg foo
  bbb
  File "dune.lock/foo.dev.pkg", line 4, characters 30-36:
  4 |  (all_platforms ((action (run ./make)))))
                                    ^^^^^^
  Error: Logs for package foo
  
  [1]
