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
  > tar cf $1.tar tmp
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
  >   (url $PWD/foo.tar)))
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
  >   (url $PWD/bar.tar)))
  > 
  > (dev)
  > EOF

Test that the project can be built normally.
  $ build_pkg foo


Make a fake dune exe:

  $ mkdir .bin
  $ cat > .bin/dune << 'EOF'
  > #!/bin/sh
  > echo "Fake dune! (args: $@)"
  > EOF
  $ chmod a+x .bin/dune

  $ dune clean

Try building in an environment where `dune` refers to the fake dune.

Remember the path to the dune under test, otherwise we would launch the wrong
Dune and add our fake `dune` to the PATH.

  $ DUNE=$(which dune)
  $ fakepath=$PWD/.bin:$PATH

Remember the digests, to not to have to call nested Dunes:

  $ foo_digest="$(dune pkg print-digest foo)"
  $ bar_digest="$(dune pkg print-digest bar)"

Call Dune with an absolute PATH as argv[0]:

  $ PATH=$fakepath $DUNE build "$pkg_root/$foo_digest/target/"
  Fake dune! (args: build -p foo @install)
  $ PATH=$fakepath $DUNE build "$pkg_root/$bar_digest/target/"
  Fake dune! (args: build -p bar @install)

Call Dune with argv[0] set to a relative PATH. Make sure "dune" in PATH refers
to the fake dune:

  $ PATH=$fakepath dune_cmd exec-a "dune" sh -c "which dune"
  $TESTCASE_ROOT/.bin/dune

Make sure that fake dune is not picked up when dune is called with argv[0] = "dune":

  $ dune clean
  $ PATH=$fakepath dune_cmd exec-a "dune" $DUNE build "$pkg_root/$foo_digest/target/"
  Fake dune! (args: build -p foo @install)
