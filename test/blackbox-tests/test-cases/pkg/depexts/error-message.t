 When a package fails to build, dune will print opam depexts warning.

  $ . ../helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

Make a library that would fail when building it:
  $ mkdir foo
  $ cat > foo/dune-project <<EOF
  > EOF
  $ tar cf foo.tar foo
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

Make dune.lock files with known program "dune".
  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build
  >  (run dune build))
  > (depexts unzip gnupg)
  > (source
  >  (fetch
  >   (url file://$PWD/foo.tar)
  >   (checksum md5=$(md5sum foo.tar | cut -f1 -d' '))))
  > EOF

Build the project, when it fails building 'foo' package, it shows the depexts
error message.
  $ dune build
  File "dune.lock/foo.pkg", line 3, characters 6-10:
  3 |  (run dune build))
            ^^^^
  Error: Logs for package foo
  File "dune-project", line 1, characters 0-0:
  Error: Invalid first line, expected: (lang <lang> <version>)
  
  Hint: You may want to verify the following depexts are installed:
  - gnupg
  - unzip
  [1]

Make dune.lock files with unknown program and unknown package.
  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build
  >  (run unknown-program))
  > (depexts unknown-package)
  > (source
  >  (fetch
  >   (url file://$PWD/foo.tar)
  >   (checksum md5=$(md5sum foo.tar | cut -f1 -d' '))))
  > EOF

Running the same build. It is supposed to show the depexts message at the end,
when the program is not found.
  $ dune build
  File "dune.lock/foo.pkg", line 3, characters 6-21:
  3 |  (run unknown-program))
            ^^^^^^^^^^^^^^^
  Error: Program unknown-program not found in the tree or in PATH
   (context: default)
  Hint: You may want to verify the following depexts are installed:
  - unknown-package
  [1]
