Cram tests can only run the binaries of lock directory packages they declare a
dependency on. They are then rerun when those binaries change.

  $ mkdir pkg && cd pkg
  $ make_lockdir
  $ make_lockpkg provider <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "printf '#!/bin/sh\necho v1\n' > helper")
  >   (system "chmod +x helper")
  >   (system "echo 'bin: [ \"helper\" ]' > provider.install")))
  > EOF
  $ make_dune_project 3.22
  $ cat >> dune-project <<EOF
  > (package (name mypkg) (allow_empty) (depends provider))
  > EOF
  $ echo '(cram)' > dune
  $ cat > t.t <<EOF
  >   \$ helper 2> /dev/null
  > EOF

The test does not depend on the package, so the binary is not found:

  $ dune test
  File "t.t", line 1, characters 0-0:
  --- t.t
  +++ t.t.corrected
  @@ -1 +1,2 @@
     $ helper 2> /dev/null
  +  [127]
  [1]

Declaring the dependency makes the binary available:

  $ echo '(cram (deps %{bin:helper}))' > dune
  $ dune test
  File "t.t", line 1, characters 0-0:
  --- t.t
  +++ t.t.corrected
  @@ -1 +1,2 @@
     $ helper 2> /dev/null
  +  v1
  [1]

When the package changes, it is rebuilt and the test is rerun:

  $ sed -i.bak 's/v1/v2/' ${default_lock_dir}/provider.pkg
  $ dune test
  File "t.t", line 1, characters 0-0:
  --- t.t
  +++ t.t.corrected
  @@ -1 +1,2 @@
     $ helper 2> /dev/null
  +  v2
  [1]

Build directory paths in the incoming PATH, as set by `eval $(dune tools env)`,
are removed, whether they hold lock directory or workspace binaries:

  $ cat > wsbin <<EOF
  > #!/bin/sh
  > echo workspace
  > EOF
  $ chmod +x wsbin
  $ cat > dune <<EOF
  > (install (section bin) (package mypkg) (files wsbin))
  > (cram)
  > EOF
  $ cat > t.t <<EOF
  >   \$ helper 2> /dev/null
  >   \$ wsbin 2> /dev/null
  > EOF
  $ dune build @install
  $ PATH="$PWD/$(get_build_pkg_dir provider)/target/bin:$PWD/_build/install/default/bin:$PATH" dune test
  File "t.t", line 1, characters 0-0:
  --- t.t
  +++ t.t.corrected
  @@ -1,2 +1,4 @@
     $ helper 2> /dev/null
  +  [127]
     $ wsbin 2> /dev/null
  +  [127]
  [1]

  $ cd ..
