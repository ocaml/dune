Clarify the behavior when the `dune` in PATH is not the one used to start the build.

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
  > (depends dune)
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
  > (depends dune)
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
  $ PATH=$fakepath $DUNE build "$pkg_root/$bar_digest/target/"

argv[0] is set by the calling program (like a shell or cram test runner) and
could be wrong, hence it cannot always be trusted. In the examples above we
launch dune with an absolute path, thus one could just use argv[0] to get the
exact path to the `dune` binary.

To make sure that we pick up the right dune even when argv[0] is being set to
unhelpful values we launch the binary but set the value to a relative value,
namely argv[0] = "dune". This is exactly what happens if `dune` is in the PATH
and the user launches `dune` in a shell.

  $ dune clean
  $ PATH=$fakepath dune_cmd exec-a "dune" $DUNE build "$pkg_root/$foo_digest/target/"
