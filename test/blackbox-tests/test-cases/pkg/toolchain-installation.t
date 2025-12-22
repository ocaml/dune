Test the installation of toolchains package by building and installing
a mock compiler package using dune's toolchain mechanism.

  $ make_lockdir

  $ mkdir fake-compiler

A configure script for the fake compiler that writes the prefix path
to a file so the fake compiler can be installed to the specified
location, testing that dune respects the "prefix" variable for
toolchain packages.
  $ cat > fake-compiler/configure << 'EOF'
  > #!/bin/sh
  > PREFIX=$1
  > echo $PREFIX > prefix.txt
  > EOF
  $ chmod a+x fake-compiler/configure

A shell script that will be installed to the toolchain bin directory
and masquerade as the compiler.
  $ mkdir -p fake-compiler/target/bin
  $ cat > fake-compiler/target/bin/ocamlc << EOF
  > #!/bin/sh
  > echo "Hello from fake ocamlc!"
  > EOF
  $ chmod a+x fake-compiler/target/bin/ocamlc

The makefile for the fake compiler copies the fake ocamlc script into
the appropriate location, respecting the DESTDIR variable.
  $ cat > fake-compiler/Makefile << 'EOF'
  > prefix := $(shell cat prefix.txt)
  > target := $(DESTDIR)$(prefix)
  > install:
  > 	@mkdir -p $(target)
  > 	@cp -r target/* $(target)
  > EOF

Lockfile for the fake compiler package:
  $ make_lockpkg ocaml-base-compiler << EOF
  > (version 1)
  > 
  > (build
  >  (run ./configure %{prefix}))
  > 
  > (install
  >  (run %{make} install))
  > 
  > (source
  >  (copy $PWD/fake-compiler))
  > EOF

  $ cat > dune-project << EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (depends ocaml-base-compiler))
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (public_name foo))
  > EOF

  $ cat > foo.ml << EOF
  > print_endline "Hello, World!"
  > EOF

Toolchain directory names contain a hash of some of the fields from
the package's lockfile. This lockfile includes the expanded $PWD
variable which includes the test sandbox path which is different each
time the tests runs. Strip the hash out of the toolchain directory
name so the output is consistent across test runs.
  $ remove_hash() {
  >   sed 's/\(ocaml-base-compiler.1-\)[^/]*/\1HASH/'
  > }

Attempt to build the project. This will fail due to the fake compiler,
but the fake compiler will end up installed as a toolchain package.
Also test that XDG_CACHE_HOME is respected.
  $ XDG_CACHE_HOME=$PWD/fake-cache DUNE_CONFIG__TOOLCHAINS=enabled build_pkg ocaml-base-compiler 2>&1 | remove_hash

Enumerate the contents of the fake toolchains directory:
  $ find fake-cache/dune/toolchains | sort | remove_hash
  fake-cache/dune/toolchains
  fake-cache/dune/toolchains/ocaml-base-compiler.1-HASH
  fake-cache/dune/toolchains/ocaml-base-compiler.1-HASH/target
  fake-cache/dune/toolchains/ocaml-base-compiler.1-HASH/target/bin
  fake-cache/dune/toolchains/ocaml-base-compiler.1-HASH/target/bin/ocamlc

Also test that DUNE_CACHE_ROOT is respected.
  $ DUNE_CACHE_ROOT=$PWD/other-fake-cache DUNE_CONFIG__TOOLCHAINS=enabled build_pkg ocaml-base-compiler 2>&1 | remove_hash

Enumerate the contents of the fake toolchains directory:
  $ find other-fake-cache/toolchains/ | sort | remove_hash
  other-fake-cache/toolchains/
  other-fake-cache/toolchains/ocaml-base-compiler.1-HASH
  other-fake-cache/toolchains/ocaml-base-compiler.1-HASH/target
  other-fake-cache/toolchains/ocaml-base-compiler.1-HASH/target/bin
  other-fake-cache/toolchains/ocaml-base-compiler.1-HASH/target/bin/ocamlc
