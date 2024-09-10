Exercises end to end locking and building a simple project.

  $ . ./helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

Make a library that would fail when building it:
  $ mkdir foo
  $ cd foo
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (
  > EOF
  $ cd ..
  $ tar -czf foo.tar.gz foo
  $ rm -rf foo

Make a package for the library:
  $ mkpkg foo <<EOF
  > depexts: [["unzip" "gnupg"]]
  > build: [
  >   [
  >     "dune"
  >     "build"
  >     "-p"
  >     name
  >     "-j"
  >     jobs
  >     "@install"
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
  4 |  (run dune build -p %{pkg-self:name} -j %{jobs} @install))
            ^^^^
  Error: Logs for package foo
  File "dune-project", line 3, characters 0-0:
  Error: unclosed parenthesis at end of input
  
  May have been due to the missing depexts:
  - unzip
  - gnupg
  (If already installed, you could ignore this message)
  [1]
