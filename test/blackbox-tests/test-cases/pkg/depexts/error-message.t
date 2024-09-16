 When a package fails to build, dune will print opam depexts warning.

  $ . ../helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

Make a library that would fail when building it:
  $ mkdir foo
  $ cat > foo/dune-project <<EOF
  > EOF
  $ tar -czf foo.tar.gz foo
  $ rm -rf foo

Make a project that uses the foo library:
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF
  $ cat > dune <<EOF
  > (executable
  >  (public_name bar)
  >  (libraries foo))
  > EOF

Make dune.lock files
  $ make_lockdir
  $ cat > dune.lock/foo.pkg <<EOF
  > (version 0.0.1)
  > (build
  >  (run dune build))
  > (depexts unzip gnupg)
  > (source
  >  (fetch
  >   (url file://$PWD/foo.tar.gz)
  >   (checksum md5=$(md5sum foo.tar.gz | cut -f1 -d' '))))
  > EOF

Build the project, when it fails building 'foo' package, it shows
the depexts error message.
  $ dune build
  File "dune.lock/foo.pkg", line 3, characters 6-10:
  3 |  (run dune build))
            ^^^^
  Error: Logs for package foo
  File "dune-project", line 1, characters 0-0:
  Error: Invalid first line, expected: (lang <lang> <version>)
  
  You may want to verify the following depexts are installed:
  - unzip
  - gnupg
  [1]
