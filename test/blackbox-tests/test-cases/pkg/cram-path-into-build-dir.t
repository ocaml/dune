Cram tests can run binaries found in PATH that point inside the build
directory, without declaring a dependency on them. The test is then not rerun
when those binaries change, and reports stale output.

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

The package changes and is rebuilt, but the test is not rerun and reports
stale output:

  $ sed -i.bak 's/v1/v2/' ${default_lock_dir}/provider.pkg
  $ build_pkg provider
  $ $(get_build_pkg_dir provider)/target/bin/helper
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
