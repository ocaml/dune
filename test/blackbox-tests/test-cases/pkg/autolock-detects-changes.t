Test that auto-locking correctly detects when to rebuild based on repository changes.

  $ . ./helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed
  $ enable_pkg

Make a library package:

  $ mkdir foo
  $ cd foo
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > EOF
  $ cat > foo.ml <<EOF
  > let message = "Hello from foo version 0.0.1!"
  > EOF
  $ cat > dune <<EOF
  > (library
  >  (public_name foo))
  > EOF
  $ cd ..
  $ tar cf foo.tar foo
  $ rm -rf foo

Make a package for the library using a local file source:

  $ mkpkg foo <<EOF
  > build: [
  >   ["dune" "build" "-p" name "@install"]
  > ]
  > url {
  >  src: "$PWD/foo.tar"
  > }
  > EOF

Create a project that depends on foo:

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF

  $ cat > bar.ml <<EOF
  > let () = print_endline Foo.message
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name bar)
  >  (libraries foo))
  > EOF

Build with auto-locking:
  $ dune exec --display short bar 2>&1 | grep "Building"
      Building foo.0.0.1

  $ dune exec bar
  Hello from foo version 0.0.1!

Add an unrelated package to the repository:
  $ mkpkg baz <<EOF
  > build: [ "echo" "baz" ]
  > EOF

Build again - adding unrelated package should NOT trigger rebuild:
  $ dune exec --display short bar 2>&1 | grep "Building" || echo "no rebuilds"
  no rebuilds

  $ dune exec bar
  Hello from foo version 0.0.1!

Now add a newer version of foo to the repository:

  $ mkdir foo
  $ cd foo
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > EOF
  $ cat > foo.ml <<EOF
  > let message = "Hello from foo 0.0.2!"
  > EOF
  $ cat > dune <<EOF
  > (library
  >  (public_name foo))
  > EOF
  $ cd ..
  $ tar cf foo-0.0.2.tar foo
  $ rm -rf foo

  $ mkpkg foo 0.0.2 <<EOF
  > build: [
  >   ["dune" "build" "-p" name "@install"]
  > ]
  > url {
  >  src: "$PWD/foo-0.0.2.tar"
  > }
  > EOF

Build again - auto-locking should detect the new version and rebuild:
  $ dune clean
  $ dune exec --display short bar 2>&1 | grep "Building"
      Building foo.0.0.2

  $ dune exec bar
  Hello from foo 0.0.2!
