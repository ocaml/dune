 When a package fails to build, dune will print opam depexts warning.

  $ mkrepo
  $ add_mock_repo_if_needed

Make a package whose source is not a dune project, so that it is built by its
recipe:
  $ mkdir foo
  $ touch foo/data
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
  $ touch bar.ml

Make dune.lock files with known program "dune".
  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build
  >  (run sh -c "echo 'the build failed' >&2; exit 1"))
  > (depexts unzip gnupg)
  > (source
  >  (fetch
  >   (url file://$PWD/foo.tar)
  >   (checksum md5=$(md5sum foo.tar | cut -f1 -d' '))))
  > EOF

Build the project, when it fails building 'foo' package, it shows the depexts
error message.
  $ dune build
  File "dune.lock/foo.pkg", line 3, characters 6-8:
  3 |  (run sh -c "echo 'the build failed' >&2; exit 1"))
            ^^
  Error: Logs for package foo
  the build failed
  
  Hint: You may want to verify the following depexts are installed: gnupg unzip
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
  unknown-package
  [1]

  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build
  >  (run unknown-program))
  > (depexts foo zoo bar baz tar jar)
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
  bar baz foo jar tar zoo
  [1]

Update the foo lockfile to have a single depext name:
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build
  >  (run sh -c "echo 'the build failed' >&2; exit 1"))
  > (depexts foo)
  > (source
  >  (fetch
  >   (url file://$PWD/foo.tar)
  >   (checksum md5=$(md5sum foo.tar | cut -f1 -d' '))))
  > EOF
Build the project, when it fails building 'foo' package, it shows the depexts
error message.
  $ dune build
  File "dune.lock/foo.pkg", line 3, characters 6-8:
  3 |  (run sh -c "echo 'the build failed' >&2; exit 1"))
            ^^
  Error: Logs for package foo
  the build failed
  
  Hint: You may want to verify the following depexts are installed: foo
  [1]

Update the foo lockfile to have short depext names:
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build
  >  (run sh -c "echo 'the build failed' >&2; exit 1"))
  > (depexts a b)
  > (source
  >  (fetch
  >   (url file://$PWD/foo.tar)
  >   (checksum md5=$(md5sum foo.tar | cut -f1 -d' '))))
  > EOF
Build the project, when it fails building 'foo' package, it shows the depexts
error message.
  $ dune build
  File "dune.lock/foo.pkg", line 3, characters 6-8:
  3 |  (run sh -c "echo 'the build failed' >&2; exit 1"))
            ^^
  Error: Logs for package foo
  the build failed
  
  Hint: You may want to verify the following depexts are installed: a b
  [1]
