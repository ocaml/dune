Testing the conflicts that could occur between the dependencies of "dune-project"
and dev-tool dependencies.

The scenario here is that the fake OCamlFormat dev-tool depends on
printer.1.0, and the project depends on a different version, printer.2.0.
It shows those two do not conflict, and the dev-tools dependencies do not leak
into the user build environment.

  $ . ./helpers.sh
  $ mkrepo

Make a fake OCamlFormat which depends on printer lib:
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
  $ tar cf ocamlformat.tar ocamlformat
  $ rm -rf ocamlformat

Make a printer lib(version 1) that prints "formatted":
  $ make_printer_lib "1.0"
  $ make_opam_printer "1.0"

Make a printer lib(version 2) that prints "Hello world!":
  $ make_printer_lib "2.0"
  $ make_opam_printer "2.0"

Make a package for the fake OCamlFormat library which depends on printer.1.0:
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
  >  src: "file://$PWD/ocamlformat.tar"
  >  checksum: [
  >   "md5=$(md5sum ocamlformat.tar | cut -f1 -d' ')"
  >  ]
  > }
  > EOF

Make dune-project that uses the mocked dev-tool opam-reposiotry.
  $ make_project_with_dev_tool_lockdir

Update the project to depends on printer.2.0:
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

Add ".ocamlformat" file.
  $ cat > .ocamlformat <<EOF
  > version = 0.26.2
  > EOF

Lock the to trigger package management
  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - printer.2.0

It shows that the project uses printer.2.0
  $ dune exec -- foo
  Hello World!

Format foo.ml, "dune fmt" uses printer.1.0 instead. There is no conflict with different
versions of the same dependency.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt --preview
  Solution for dev-tools.locks/ocamlformat:
  - ocamlformat.0.26.2
  - printer.1.0
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  [1]
  $ cat _build/default/.formatted/foo.ml
  formatted

Update "dune-project", removing the dependency on the "printer" package. This
demonstrates that even though OCamlFormat depends on the "printer" package, building the
project will not work because foo's dependency on the library "printer" (specified in
the "dune" file) cannot be resolved. This is because dependencies of dev-tools and
dependencies of the project are isolated from one another.
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name foo))
  > EOF

Relock the project.
  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  (no dependencies to lock)

There is no leak here. It is not taking the "printer" lib from dev-tools.
  $ dune exec -- foo
  File "dune", line 3, characters 12-19:
  3 |  (libraries printer))
                  ^^^^^^^
  Error: Library "printer" not found.
  -> required by _build/default/.foo.eobjs/native/dune__exe__Foo.cmx
  -> required by _build/default/foo.exe
  -> required by _build/install/default/bin/foo
  [1]

Update the executable "foo" to not depend on the library "printer", but "foo.ml" still
refers to the "Printer" module. This won't compile, demonstrating that modules from
dev-tools don't leak into the project.
  $ cat > dune <<EOF
  > (executable
  >  (public_name foo))
  > EOF

There is no leak here. It is not taking Printer module from the printer of dev-tools dependency.
  $ dune exec -- foo
  File "foo.ml", line 1, characters 9-16:
  1 | let () = Printer.print ()
               ^^^^^^^
  Error: Unbound module Printer
  Hint:    Did you mean Printexc or Printf?
  [1]
