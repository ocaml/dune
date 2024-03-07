Clarify the behavior when the `dune` in PATH is not the one used to start the build.

  $ . ./helpers.sh

  $ make_test_package() {
  >   mkdir tmp
  >   cd tmp
  >   cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name $1)
  >  (allow_empty))
  > EOF
  > cd ..
  > tar -czf $1.tar.gz tmp
  > rm -rf tmp
  > }

  $ make_test_package foo
  $ make_test_package bar

Make a project that depends on the test packages:

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends foo bar))
  > EOF

  $ cat > dune <<EOF
  > (data_only_dirs bin)
  > EOF

Make lockfiles for the packages.
  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > 
  > (build
  >  (run dune build -p %{pkg-self:name} @install))
  > 
  > (source
  >  (fetch
  >   (url $PWD/foo.tar.gz)))
  > 
  > (dev)
  > EOF

  $ make_lockpkg bar <<EOF
  > (version 0.0.1)
  > 
  > (build
  >   ; Exercise that the dune exe can be located when it's launched by a subprocess.
  >  (run sh -c "dune build -p %{pkg-self:name} @install"))
  > 
  > (source
  >  (fetch
  >   (url $PWD/bar.tar.gz)))
  > 
  > (dev)
  > EOF

Test that the project can be built normally.
  $ dune build

Make a fake dune exe:
  $ mkdir bin
  $ cat > bin/dune <<EOF
  > #!/bin/sh
  > echo "Fake dune! (args: \$@)"
  > EOF
  $ chmod a+x bin/dune

  $ dune clean
Try building in an environment where `dune` refers to the fake dune.
  $ DUNE=$(which dune)  # otherwise we would start by running the wrong dune
  $ PATH=$PWD/bin:$PATH $DUNE build
  Fake dune! (args: build -p bar @install)
  Fake dune! (args: build -p foo @install)
