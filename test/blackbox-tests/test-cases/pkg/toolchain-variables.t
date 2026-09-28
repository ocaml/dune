Test the installation of toolchains package by building and installing
a mock compiler package using dune's toolchain mechanism.

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

The dependent recipe uses a shared layout even when its provider installs
outside the build directory. Its file capabilities must point at that layout.

  $ make_lockpkg baz << EOF
  > (version 1)
  > (build
  >  (progn
  >   (run sh -exc "echo %{pkg:ocaml-base-compiler:share}")
  >   (run sh %{pkg:ocaml-base-compiler:share_root:bin/ocamlc})))
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

The recipe receives sandbox-relative layout paths and can read the installed
file through its direct dependency's capability.

  $ XDG_CACHE_HOME=$PWD/fake-cache dune build @pkg-install 2>&1 \
  > | dune_cmd subst '[[:alnum:]]{32}' '<hash>' \
  > | dune_cmd subst '[^ ]*_build' '$TESTCASE_ROOT/_build'
  $TESTCASE_ROOT/_build/.sandbox/<hash>/_private/default/.packages/<hash>/share/ocaml-base-compiler
  Hello from fake ocamlc!

