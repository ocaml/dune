Testing the conflicts that could occurs between the dependencies of dune-projects
and dev-tools dependencies.

The scenario here is, the fake ocamlforamt dev-tool depends on
printer.1.0 and the project depends on a different version, printer.2.0.
It shows those 2 does not conflicts and the dev tools dependencies do not leak
into the user build environment.

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
  > let () = Printer.print ()
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name ocamlformat)
  >  (libraries printer))
  > EOF

  $ cd ..
  $ tar -czf ocamlformat.tar.gz ocamlformat
  $ rm -rf ocamlformat

Make a printer lib(version 1) that prints "formatted":
  $ mkdir printer
  $ cd printer
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package (name printer))
  > EOF

  $ cat > printer.ml <<EOF
  > let print () = print_endline "formatted"
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (public_name printer))
  > EOF

  $ cd ..
  $ tar -czf printer.1.tar.gz printer

Make a printer lib(version 2) that prints "Hello world":
  $ cd printer
  $ cat > printer.ml <<EOF
  > let print () = print_endline "Hello world!"
  > EOF
  $ cd ..
  $ tar -czf printer.2.tar.gz printer
  $ rm -rf printer

A printer 1.0 into opam-repository
  $ mkpkg printer 1.0 <<EOF
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
  >  src: "file://$PWD/printer.1.tar.gz"
  >  checksum: [
  >   "md5=$(md5sum printer.1.tar.gz | cut -f1 -d' ')"
  >  ]
  > }
  > EOF

Add printer.2.0 into opam-repository
  $ mkpkg printer 2.0 <<EOF
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
  >  src: "file://$PWD/printer.2.tar.gz"
  >  checksum: [
  >   "md5=$(md5sum printer.2.tar.gz | cut -f1 -d' ')"
  >  ]
  > }
  > EOF

Make a package for the fake ocamlformat library which depends on printer.1.0:
  $ mkpkg ocamlformat 0.26.2 <<EOF
  > depends: [
  >  "printer" {= "1.0"}
  > ]
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
  >  src: "file://$PWD/ocamlformat.tar.gz"
  >  checksum: [
  >   "md5=$(md5sum ocamlformat.tar.gz | cut -f1 -d' ')"
  >  ]
  > }
  > EOF

Make a project that uses the fake ocamlformat:

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name foo)
  >  (depends (printer (= 2.0))))
  > EOF

  $ cat > foo.ml <<EOF
  > let () = Printer.print ()
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name foo)
  >  (libraries printer))
  > EOF

  $ cat > .ocamlformat <<EOF
  > version = 0.26.2
  > EOF

  $ cat > dune-workspace <<EOF
  > (lang dune 3.13)
  > (lock_dir
  >  (path "dev-tools.locks/ocamlformat")
  >  (repositories mock))
  > (lock_dir
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > EOF


Lock the to trigger package management
  $ dune pkg lock
  Solution for dune.lock:
  - printer.2.0

It shows that the project uses printer.2.0
  $ dune exec -- foo
  Hello world!

Format foo.ml, 'dune fmt' uses printer.1.0 instead, there is no conclit with different 
version of the same dependency.
  $ dune fmt
  Solution for dev-tools.locks/ocamlformat:
  - ocamlformat.0.26.2
  - printer.1.0
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  Promoting _build/default/.formatted/foo.ml to foo.ml.
  [1]
  $ cat foo.ml
  formatted

Revert foo.ml
  $ cat > foo.ml <<EOF
  > let () = Printer.print ()
  > EOF

Now dune-project does not depend on printer, but the executable foo depends on it.
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name foo))
  > EOF

Lock before formating
  $ dune pkg lock
  Solution for dune.lock:
  (no dependencies to lock)
  $ dune fmt
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  Promoting _build/default/.formatted/foo.ml to foo.ml.
  [1]
  $ cat foo.ml
  formatted

Revert foo.ml
  $ cat > foo.ml <<EOF
  > let () = Printer.print ()
  > EOF

There is no leak here, it is not taking the printer lib from dev-tools
  $ dune clean && dune exec -- foo
  File "dune", line 3, characters 12-19:
  3 |  (libraries printer))
                  ^^^^^^^
  Error: Library "printer" not found.
  -> required by _build/default/.foo.eobjs/byte/dune__exe__Foo.cmi
  -> required by _build/default/.foo.eobjs/native/dune__exe__Foo.cmx
  -> required by _build/default/foo.exe
  -> required by _build/install/default/bin/foo
  [1]

Now the executable foo does not depends on printer, but foo.ml always uses Printer module from
printer lib.
  $ cat > dune <<EOF
  > (executable
  >  (public_name foo))
  > EOF

We ensure that it solving
  $ dune fmt
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  Promoting _build/default/.formatted/foo.ml to foo.ml.
  [1]
  $ cat foo.ml
  formatted

Revert foo.ml
  $ cat > foo.ml <<EOF
  > let () = Printer.print ()
  > EOF

There is no leak here, it is not taking Printer module from the printer of dev-tools dependency
  $ dune clean && dune exec -- foo
  File "foo.ml", line 1, characters 9-22:
  1 | let () = Printer.print ()
               ^^^^^^^^^^^^^
  Error: Unbound module Printer
  Hint: Did you mean Printexc or Printf?
  [1]
