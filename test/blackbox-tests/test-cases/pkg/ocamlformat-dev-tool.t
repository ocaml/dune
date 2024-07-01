Exercises end to end, locking and building ocamlformat dev tool.

  $ . ./helpers.sh
  $ mkrepo

Make a library:
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
  >   "md5=$(md5sum ocamlformat.tar.gz | cut -f1 -d' ')"
  >  ]
  > }
  > EOF

Make a project that uses the library:

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
  > profile = default
  > version = 0.26.2
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.13)
  > (lock_dir
  >  (path "dune.lock/dev_tools.lock/ocamlformat_dev")
  >  (repositories mock))
  > (lock_dir
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > EOF

Lock, build, and run the `dune fmt` command in the project:

We lock the to trigger package management 
  $ dune pkg lock
  Solution for dune.lock:
  (no dependencies to lock)

We format the foo.ml
  $ dune fmt
  Solution for dune.lock/dev_tools.lock/ocamlformat_dev:
  - ocamlformat.0.26.2
  Error: unknown checksum md5=0f438cf036709326e9e0d82ea9eaf2ec
  -> required by _build/_private/default/.ocamlformat/ocamlformat/source
  -> required by _build/_private/default/.ocamlformat/ocamlformat/target/cookie
  -> required by _build/default/.formatted/foo.ml
  -> required by alias .formatted/fmt
  -> required by alias fmt
  [1]
  $ cat foo.ml
  let () = print_endline "Hello, world"

We format again, ocamlformat is a dependency now
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name foo)
  > (depends ocamlformat))
  > EOF
  $ cat > foo.ml <<EOF
  > let () = print_endline "Hello, world"
  > EOF

We lock the to trigger package management 
  $ dune pkg lock
  Solution for dune.lock:
  - ocamlformat.0.26.2

We format the foo.ml
  $ dune fmt
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  Promoting _build/default/.formatted/foo.ml to foo.ml.
  [1]
  $ cat foo.ml
  formatted
