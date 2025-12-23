Demonstrate our support for pin-depends.

  $ add_mock_repo_if_needed
  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF
  $ mkrepo
  $ mkpkg bar 0.0.1

  $ runtest() {
  >   cat > foo.opam <<EOF
  >   opam-version: "2.0"
  >   depends: [ "bar" ]
  >   pin-depends: [ "bar.1.0.0" "$1" ]
  > EOF
  >   dune_pkg_lock_normalized && {
  >     local pkg="${default_lock_dir}/bar.1.0.0.pkg"
  >     grep version $pkg
  >     grep dev $pkg
  >     print_source "bar.1.0.0"
  >   } 
  > }

Local pinned source.

"opam" file at the root

  $ dir=_bar_file
  $ mkdir $dir
  $ cat >$dir/opam <<EOF
  > opam-version: "2.0"
  > EOF
  $ runtest "file://$PWD/$dir"
  Solution for dune.lock:
  - bar.1.0.0
  (version 1.0.0)
  (dev)
  (source (fetch (url file://PWD/_bar_file)))

"opam" directory at the root

  $ dir=_bar_file_opam_dir
  $ mkdir -p $dir/opam
  $ cat >$dir/opam/bar.opam <<EOF
  > opam-version: "2.0"
  > EOF
  $ runtest "file://$PWD/$dir"
  Solution for dune.lock:
  - bar.1.0.0
  (version 1.0.0)
  (dev)
  (source (fetch (url file://PWD/_bar_file_opam_dir)))

"bar.opam" file at the root

  $ dir=_bar_named_opam_root
  $ mkdir $dir
  $ cat >$dir/bar.opam <<EOF
  > opam-version: "2.0"
  > EOF
  $ runtest "file://$PWD/$dir"
  Solution for dune.lock:
  - bar.1.0.0
  (version 1.0.0)
  (dev)
  (source (fetch (url file://PWD/_bar_named_opam_root)))

"bar.opam" file at opam/

  $ dir=_bar_named_opam_subdir
  $ mkdir -p $dir/opam
  $ cat >$dir/opam/bar.opam <<EOF
  > opam-version: "2.0"
  > EOF
  $ runtest "file://$PWD/$dir"
  Solution for dune.lock:
  - bar.1.0.0
  (version 1.0.0)
  (dev)
  (source (fetch (url file://PWD/_bar_named_opam_subdir)))

Git pinned source:

  $ dir=_bar_git
  $ mkdir $dir
  $ cd $dir
  $ git init --quiet
  $ cat >opam <<EOF
  > opam-version: "2.0"
  > EOF
  $ git add -A
  $ git commit --quiet -m "Initial commit"
  $ cd ..
  $ runtest "git+file://$PWD/$dir"
  Solution for dune.lock:
  - bar.1.0.0
  (version 1.0.0)
  (dev)
  (source (fetch (url git+file://PWD/_bar_git)))

Git pinned source with toplevel opam file:

  $ dir=_bar_opam_git
  $ mkdir $dir
  $ cd $dir
  $ git init --quiet
  $ cat >bar.opam <<EOF
  > opam-version: "2.0"
  > EOF
  $ git add -A
  $ git commit --quiet -m "Initial commit"
  $ cd ..
  $ runtest "git+file://$PWD/$dir"
  Solution for dune.lock:
  - bar.1.0.0
  (version 1.0.0)
  (dev)
  (source (fetch (url git+file://PWD/_bar_opam_git)))

Git pinned source with toplevel opam dir 1

  $ dir=_bar_opam_dir_git1
  $ mkdir $dir
  $ cd $dir
  $ git init --quiet
  $ mkdir opam
  $ cat >opam/opam <<EOF
  > opam-version: "2.0"
  > EOF
  $ git add -A
  $ git commit --quiet -m "Initial commit"
  $ cd ..
  $ runtest "git+file://$PWD/$dir"
  Solution for dune.lock:
  - bar.1.0.0
  (version 1.0.0)
  (dev)
  (source (fetch (url git+file://PWD/_bar_opam_dir_git1)))

Git pinned source with toplevel opam dir 2

  $ dir=_bar_opam_dir_git2
  $ mkdir $dir
  $ cd $dir
  $ git init --quiet
  $ mkdir opam
  $ cat >opam/foo.opam <<EOF
  > opam-version: "2.0"
  > EOF
  $ git add -A
  $ git commit --quiet -m "Initial commit"
  $ cd ..
  $ runtest "git+file://$PWD/$dir"
  File "foo.opam", line 1, characters 0-0:
  Error: unable to discover an opam file for package bar
  [1]

Pin to something that doesn't have an opam file

  $ dir=_no_opam; mkdir $dir
  $ runtest "file://$PWD/$dir"
  File "foo.opam", line 1, characters 0-0:
  Error: unable to discover an opam file for package bar
  [1]

Pin to an invalid opam file

  $ dir=_invalid_opam; mkdir $dir
  $ touch $dir/opam
  $ runtest "file://$PWD/$dir"
  File "$TESTCASE_ROOT/_invalid_opam/opam", line 1, characters 0-0:
  Error: unexpected version
  unsupported or missing file format version; should be 2.0 or older
  [1]

Pin to an HTTP archive work

  $ mkdir _source/
  $ cat > _source/bar.opam << EOF
  > opam-version: "2.0"
  > EOF
  $ tar cf tarball.tar -C _source bar.opam
  $ MD5_CHECKSUM=$(md5sum tarball.tar  | cut -f1 -d' ')
  $ echo tarball.tar > fake-curls
  $ PORT=1
  $ runtest "http://0.0.0.0:$PORT/tarball.tar" > output
  $ grep "md5=$MD5_CHECKSUM" output 2>&1 > /dev/null && echo "Checksum matches"
  Checksum matches

Pin to an HTTP archive detects wrong hash

  $ cat << EOF > dune
  > (library
  >  (name foo)
  >  (libraries bar))
  > EOF
  $ dune_cmd subst "$MD5_CHECKSUM" '92449184682b45b5f07e811fdd61d35f' ${default_lock_dir}/bar.1.0.0.pkg
  $ rm -rf already-served
  $ dune build 2>&1 | grep -v "md5"
  File "dune.lock/bar.1.0.0.pkg", line 6, characters 12-48:
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Invalid checksum, got

Pin to an HTTP archive needs `dune_pkg_lock_normalized` to download and compute the hash
of the target again

  $ rm tarball.tar already-served
  $ echo "update checksum" > _source/random_file
  $ tar cf tarball.tar -C _source bar.opam random_file
  $ MD5_CHECKSUM=$(md5sum tarball.tar  | cut -f1 -d' ')
  $ echo tarball.tar > fake-curls
  $ runtest "http://0.0.0.0:$PORT/tarball.tar" > output
  $ grep "md5=$MD5_CHECKSUM" output 2>&1 > /dev/null && echo "Checksum matches"
  Checksum matches

