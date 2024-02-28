Exercises end to end locking and building a simple project.

  $ . ./helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

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
  $ tar -czf foo.tar.gz foo
  $ rm -rf foo

Start a oneshot webserver so dune can download the packgae with http:
  $ webserver_oneshot --content-file foo.tar.gz --port-file port.txt &
  $ until test -f port.txt; do sleep 0.1; done
  $ PORT=$(cat port.txt)

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
  >   "md5=$(md5sum foo.tar.gz | cut -f1 -d' ')"
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

Lock, build, and run the executable in the project:

  $ dune pkg lock
  Solution for dune.lock:
  - foo.0.0.1

# Temporary failure until 10080 is fixed

  $ dune exec bar
  Hello, World!

  $ wait
