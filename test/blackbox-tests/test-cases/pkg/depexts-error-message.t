 When a package fails to build, dune will print opam depexts warning.

  $ . ./helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

Make a library that would fail when building it:
  $ mkdir foo
  $ cat > foo/dune-project <<EOF
  > EOF
  $ tar -czf foo.tar.gz foo
  $ rm -rf foo

Make a package for the library with depexts:
  $ mkpkg foo <<EOF
  > depexts: [["unzip" "gnupg"]]
  > build: [
  >   [
  >     "dune"
  >     "build"
  >     "-p"
  >     name
  >   ]
  > ]
  > url {
  >  src: "file://$PWD/foo.tar.gz"
  >  checksum: [
  >   "md5=$(md5sum foo.tar.gz | cut -f1 -d' ')"
  >  ]
  > }
  > EOF

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

Lock, build, and run the executable in the project:
  $ dune pkg lock
  Solution for dune.lock:
  - foo.0.0.1
  $ dune build
  File "dune.lock/foo.pkg", line 4, characters 6-10:
  4 |  (run dune build -p %{pkg-self:name}))
            ^^^^
  Error: Logs for package foo
  File "dune-project", line 1, characters 0-0:
  Error: Invalid first line, expected: (lang <lang> <version>)
  
  May have been due to the missing depexts:
  - unzip
  - gnupg
  (If already installed, you could ignore this message)
  [1]
