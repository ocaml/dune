Demonstrate our support for pin-depends.

  $ . ./helpers.sh

  $ add_mock_repo_if_needed
  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF
  $ mkrepo
  $ mkpkg bar 0.0.1

  $ runtest() {
  > cat >foo.opam <<EOF
  > opam-version: "2.0"
  > depends: [ "bar" ]
  > pin-depends: [ "bar.1.0.0" "$1" ]
  > EOF
  > dune pkg lock && {
  >   local pkg="dune.lock/bar.pkg";
  >   grep version $pkg;
  >   grep dev $pkg;
  >   print_source "bar";
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
  (source (fetch (url file://PWD/_bar_file))) (dev) 

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
  (source (fetch (url file://PWD/_bar_file_opam_dir))) (dev) 

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
  (source (fetch (url file://PWD/_bar_named_opam_root))) (dev) 

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
  (source (fetch (url file://PWD/_bar_named_opam_subdir))) (dev) 

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
  (source (fetch (url git+file://PWD/_bar_git))) (dev) 

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
  (source (fetch (url git+file://PWD/_bar_opam_git))) (dev) 

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
  (source (fetch (url git+file://PWD/_bar_opam_dir_git1))) (dev) 

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

Pin to an HTTP archive doesn't work

  $ runtest "http://0.0.0.0/tarball.tgz"
  File "foo.opam", line 1, characters 0-0:
  Error: Could not determine location of repository http://0.0.0.0/tarball.tgz
  Hint: Specify either a file path or git repo via SSH/HTTPS
  [1]
