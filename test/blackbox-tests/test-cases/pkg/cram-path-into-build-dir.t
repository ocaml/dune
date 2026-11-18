Cram tests can run binaries found in PATH that point inside the build
directory, without declaring a dependency on them. The test is then not rerun
when those binaries change, and reports stale output.

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

The test does not depend on the package, so the package is not built and the
binary is not found:

  $ dune test
  File "t.t", line 1, characters 0-0:
  --- t.t
  +++ t.t.corrected
  @@ -1 +1,2 @@
     $ helper 2> /dev/null
  +  [127]
  [1]

Building the package does not cause the test to be rerun:

  $ build_pkg provider
  $ dune test
  File "t.t", line 1, characters 0-0:
  --- t.t
  +++ t.t.corrected
  @@ -1 +1,2 @@
     $ helper 2> /dev/null
  +  [127]
  [1]

Once the test is rerun for another reason, it finds the binary, which shows
that the outcome depends on what happens to be built:

  $ echo '  $ true' >> t.t
  $ dune test
  File "t.t", line 1, characters 0-0:
  --- t.t
  +++ t.t.corrected
  @@ -1,2 +1,3 @@
     $ helper 2> /dev/null
  +  v1
     $ true
  [1]

Change the helper source without editing the lock entry. The rebuilt helper
prints v2 at the same path, but the test still reports v1:

  $ helper="$(get_build_pkg_dir provider)/target/bin/helper"
  $ printf '#!/bin/sh\necho v2\n' | make_lockpkg_file provider helper
  $ build_pkg provider
  $ "$helper"
  v2
  $ dune test
  File "t.t", line 1, characters 0-0:
  --- t.t
  +++ t.t.corrected
  @@ -1,2 +1,3 @@
     $ helper 2> /dev/null
  +  v1
     $ true
  [1]

  $ cd ..
