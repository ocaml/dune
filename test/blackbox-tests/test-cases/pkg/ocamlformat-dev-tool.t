Exercises end to end, locking and building ocamlformat dev tool.

  $ . ./helpers.sh
  $ mkrepo

Make a fake ocamlformat:
  $ mkdir ocamlformat
  $ cd ocamlformat
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package (name ocamlformat))
  > EOF

  $ cat > ocamlformat.ml <<EOF
  > let () = print_endline "formatted"
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name ocamlformat))
  > EOF

  $ cd ..
  $ tar -czf ocamlformat.tar.gz ocamlformat
  $ rm -rf ocamlformat

Start a oneshot webserver so dune can download the packgae with http:
  $ webserver_oneshot --content-file ocamlformat.tar.gz --port-file port.txt &
  $ until test -f port.txt; do sleep 0.1; done
  $ PORT=$(cat port.txt)

Make a package for the library:
  $ mkpkg ocamlformat 0.26.2 <<EOF
  > build: [
  >   [
  >     "dune"
  >     "build"
  >     "-p"
  >     name
  >     "@install"
  >   ]
  > ]
  > url {
  >  src: "http://127.0.0.1:$PORT"
  >  checksum: [
  >   "md5=$(md5sum ocamlformat.tar.gz | cut -f1 -d' ')"
  >  ]
  > }
  > EOF

Make a project that uses the fake ocamlformat:

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name foo))
  > EOF

  $ cat > foo.ml <<EOF
  > let () = print_endline "Hello, world"
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name foo))
  > EOF

  $ cat > .ocamlformat <<EOF
  > version = 0.26.2
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.13)
  > (lock_dir
  >  (path "dev_tools/ocamlformat.lock")
  >  (repositories mock))
  > (lock_dir
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > EOF

Lock, build, and run the `dune fmt` command in the project:

Lock the to trigger package management
  $ dune pkg lock
  Solution for dune.lock:
  (no dependencies to lock)

Format the foo.ml
  $ dune fmt
  Solution for dev_tools/ocamlformat.lock:
  - ocamlformat.0.26.2
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  Promoting _build/default/.formatted/foo.ml to foo.ml.
  [1]
  $ cat foo.ml
  formatted

The second time, it is not supposed to solve again.
  $ dune fmt

When dev_tools.locks is removed, the solving is renewed
  $ rm -r dev_tools/ocamlformat.lock
  $ dune fmt
  Solution for dev_tools/ocamlformat.lock:
  - ocamlformat.0.26.2

Format again, ocamlformat is a dependency with different version, to make sure it works
without pkg management
  $ webserver_oneshot --content-file ocamlformat.tar.gz --port-file port.txt &
  $ until test -f port.txt; do sleep 0.1; done
  $ PORT=$(cat port.txt)
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name foo)
  >  (depends (ocamlformat (= 0.26.3))))
  > EOF
  $ cat > foo.ml <<EOF
  > let () = print_endline "Hello, world"
  > EOF
Create another version of ocamlformat and the new webserver PORT for the URL.
  $ mkpkg ocamlformat 0.26.3 <<EOF
  > build: [
  >   [
  >     "dune"
  >     "build"
  >     "-p"
  >     name
  >     "@install"
  >   ]
  > ]
  > url {
  >  src: "http://127.0.0.1:$PORT"
  >  checksum: [
  >   "md5=$(md5sum ocamlformat.tar.gz | cut -f1 -d' ')"
  >  ]
  > }
  > EOF

Lock the to trigger package management with the new version
  $ dune pkg lock
  Solution for dune.lock:
  - ocamlformat.0.26.3

Format the foo.ml
  $ dune fmt
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  Promoting _build/default/.formatted/foo.ml to foo.ml.
  [1]
  $ cat foo.ml
  formatted
