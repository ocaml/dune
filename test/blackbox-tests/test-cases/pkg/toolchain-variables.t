Test the installation of toolchains package by building and installing
a mock compiler package using dune's toolchain mechanism.

  $ . ./helpers.sh
  $ make_lockdir
  $ add_mock_repo_if_needed

We create a fake compiler by creating a configure file.

  $ mkdir fake-compiler
  $ cat > fake-compiler/configure << 'EOF'
  > #!/bin/sh
  > PREFIX=$1
  > echo $PREFIX > prefix.txt
  > EOF
  $ chmod a+x fake-compiler/configure

We add a shell script to be installed as a fake compiler

  $ mkdir -p fake-compiler/target/share/bin
  $ cat > fake-compiler/target/share/bin/ocamlc << EOF
  > #!/bin/sh
  > echo "Hello from fake ocamlc!"
  > EOF
  $ chmod a+x fake-compiler/target/share/bin/ocamlc

We make sure the installed script is installing the script at the correct
location

  $ cat > fake-compiler/make << 'EOF'
  > #!/bin/sh
  > prefix=$(cat prefix.txt)
  > target=${DESTDIR}${prefix}
  > install() {
  >   mkdir -p "${target}"
  >   cp -r target/* "${target}"
  > }
  > install
  > EOF
  $ chmod +x fake-compiler/make

We generate the lockfile for the fake compiler

  $ make_lockpkg ocaml-base-compiler << EOF
  > (version 1)
  > (build
  >  (run ./configure %{prefix}))
  > (install
  >  (run ./make install))
  > (source
  >  (copy $PWD/fake-compiler))
  > EOF

We generate the lock file for the package to demonstrate the variable is
replaced with the path to the sandbox inside of the path to the non-relocatable
location.

  $ make_lockpkg baz << EOF
  > (version 1)
  > (build
  >  (run sh -exc "echo %{pkg:ocaml-base-compiler:share}"))
  > (depends ocaml-base-compiler)
  > EOF

We generate a fake package to use it

  $ cat > dune-project << EOF
  > (lang dune 3.18)
  > (package
  >  (name foo)
  >  (depends ocaml-base-compiler baz))
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (public_name foo))
  > EOF

  $ cat > foo.ml << EOF
  > print_endline "Hello, World!"
  > EOF

We try to build the dependency to show that it echoes the wong path. Until we
fix the problem, it shows the sandbox path

  $ XDG_CACHE_HOME=$PWD/fake-cache dune build @pkg-install 2>&1 | sed -E 's#[[:alnum:]]{32}#<hash>#g' | sed 's#[^ ]*_build#$TESTCASE_ROOT/_build#g'
  $TESTCASE_ROOT/_build/.sandbox/<hash>/_private/default/.pkg/ocaml-base-compiler/target/share/ocaml-base-compiler

