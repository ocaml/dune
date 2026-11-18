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

  $ cd ..
