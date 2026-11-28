Cram tests can only run the binaries of lock directory packages they declare a
dependency on. They are then rerun when those binaries change.

  $ mkdir pkg && cd pkg
  $ make_lockdir
  $ make_lockpkg provider <<'EOF'
  > (version 0.0.1)
  > (install
  >  (progn
  >   (run cp helper %{pkg-self:bin}/helper)
  >   (run chmod +x %{pkg-self:bin}/helper)))
  > EOF
  $ printf '#!/bin/sh\necho v1\n' | make_lockpkg_file provider helper
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

Change the helper source without editing the lock entry. The package is
rebuilt and the test is rerun:

  $ helper="$(get_build_pkg_dir provider)/target/bin/helper"
  $ printf '#!/bin/sh\necho v2\n' | make_lockpkg_file provider helper
  $ build_pkg provider
  $ "$helper"
  v2
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

Entries are compared as directories rather than spellings: the build directory
reached through [.], or through a symlink, is removed as well, while a directory
outside of it reached through [_build/..] is kept:

  $ mkdir host-bin
  $ cat > host-bin/hosttool <<EOF
  > #!/bin/sh
  > echo host
  > EOF
  $ chmod +x host-bin/hosttool
  $ ln -s _build/install/default/bin linked-bin
  $ cat > t.t <<EOF
  >   \$ helper 2> /dev/null
  >   \$ wsbin 2> /dev/null
  >   \$ hosttool 2> /dev/null
  > EOF
  $ PATH="$PWD/./$(get_build_pkg_dir provider)/target/bin:$PWD/linked-bin:$PWD/_build/../host-bin:$PATH" dune test
  File "t.t", line 1, characters 0-0:
  --- t.t
  +++ t.t.corrected
  @@ -1,3 +1,6 @@
     $ helper 2> /dev/null
  +  [127]
     $ wsbin 2> /dev/null
  +  [127]
     $ hosttool 2> /dev/null
  +  host
  [1]

This only applies to build rules. Commands run on behalf of the user, such as
`dune exec`, keep the caller's PATH, including dev tools installed in the build
directory:

  $ mkdir -p _build/extra-bin
  $ cat > _build/extra-bin/extratool <<EOF
  > #!/bin/sh
  > echo extra
  > EOF
  $ chmod +x _build/extra-bin/extratool
  $ PATH="$PWD/_build/extra-bin:$PATH" dune exec -- sh -c extratool
  extra

  $ cd ..
