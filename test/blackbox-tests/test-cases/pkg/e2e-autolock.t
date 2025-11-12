Same setup as e2e.t but this time using building without an explicit
`dune pkg lock`.

  $ . ./helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed
  $ enable_pkg

Make a library:
  $ mkdir foo
  $ cd foo
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > EOF
  $ cat > foo.ml <<EOF
  > let foo = "Hello, World!"
  > EOF
  $ cat > dune <<EOF
  > (library
  >  (public_name foo))
  > EOF
  $ cd ..
  $ tar cf foo.tar foo
  $ rm -rf foo

Configure our fake curl to serve the tarball

  $ echo foo.tar >> fake-curls
  $ PORT=1

Make a package for the library:
  $ mkpkg foo <<EOF
  > build: [
  >   ["dune" "subst"] {dev}
  >   [
  >     "dune"
  >     "build"
  >     "-p"
  >     name
  >     "-j"
  >     jobs
  >     "@install"
  >     "@runtest" {with-test}
  >     "@doc" {with-doc}
  >   ]
  > ]
  > url {
  >  src: "http://0.0.0.0:$PORT"
  >  checksum: [
  >   "md5=$(md5sum foo.tar | cut -f1 -d' ')"
  >  ]
  > }
  > EOF

Make a project that uses the library:

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF

  $ cat > bar.ml <<EOF
  > let () = print_endline Foo.foo
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name bar)
  >  (libraries foo))
  > EOF

Lock, build, and run the executable in the project (without dune pkg lock):

  $ dune exec bar
  Hello, World!

