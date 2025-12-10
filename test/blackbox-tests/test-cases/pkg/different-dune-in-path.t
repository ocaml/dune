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
  >  (depends foo bar))
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name exec_a)
  >  (public_name exec-a)
  >  (libraries unix))
  > EOF

Small helper binary to implement `exec -a` in a portable way

  $ cat > exec_a.ml <<EOF
  > let () =
  >   let desired_argv0 = Sys.argv.(1) in
  >   let prog = Sys.argv.(2) in
  >   let args = Array.length Sys.argv in
  >   let new_args = Array.init (args - 2) (fun i -> Sys.argv.(i+2)) in
  >   new_args.(0) <- desired_argv0;
  >   Unix.execvp prog new_args
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

  $ dune build @install
  $ mkdir .temp
  $ dune install --prefix $PWD/.temp
  $ cp .temp/bin/exec-a $PWD/.bin/

Show that the binary works like `exec -a` would work

  $ .bin/exec-a "desired-argv0" sh -c 'echo $0'
  desired-argv0

  $ dune clean

Try building in an environment where `dune` refers to the fake dune.

Remember the path to the dune under test:

  $ DUNE=$(which dune)  # otherwise we would start by running the wrong dune

Remember the digests, to not to have to call nested Dunes:

  $ foo_digest="$(dune pkg print-digest foo)"
  $ bar_digest="$(dune pkg print-digest bar)"

Call Dune with an absolute PATH as argv[0]:

  $ PATH=$PWD/.bin:$PATH $DUNE build "$pkg_root/$foo_digest/target/"
  Fake dune! (args: build -p foo @install)
  $ PATH=$PWD/.bin:$PATH $DUNE build "$pkg_root/$bar_digest/target/"
  Fake dune! (args: build -p bar @install)

Call Dune with argv[0] set to a relative PATH. Make sure "dune" in PATH refers
to the fake dune:

  $ (PATH=$PWD/.bin:$PATH exec-a "dune" sh -c "which dune")
  $TESTCASE_ROOT/.bin/dune

Make sure that fake dune is not picked up when dune is called with argv[0] = "dune":

  $ dune clean
  $ (PATH=$PWD/.bin:$PATH exec-a "dune" $DUNE build "$pkg_root/$foo_digest/target/")
  Fake dune! (args: build -p foo @install)
