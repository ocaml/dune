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
  > dune pkg lock
  > cat dune.lock/bar.pkg
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
  - bar.0.0.1
  (version 0.0.1)

"opam" directory at the root

  $ dir=_bar_file_opam_dir
  $ mkdir -p $dir/opam
  $ cat >$dir/opam/bar.opam <<EOF
  > opam-version: "2.0"
  > EOF
  $ runtest "file://$PWD/$dir"
  Solution for dune.lock:
  - bar.0.0.1
  (version 0.0.1)

"bar.opam" file at the root

  $ dir=_bar_named_opam_root
  $ mkdir $dir
  $ cat >$dir/bar.opam <<EOF
  > opam-version: "2.0"
  > EOF
  $ runtest "file://$PWD/$dir"
  Solution for dune.lock:
  - bar.0.0.1
  (version 0.0.1)

"bar.opam" file at opam/

  $ dir=_bar_named_opam_subdir
  $ mkdir -p $dir/opam
  $ cat >$dir/opam/bar.opam <<EOF
  > opam-version: "2.0"
  > EOF
  $ runtest "file://$PWD/$dir"
  Solution for dune.lock:
  - bar.0.0.1
  (version 0.0.1)

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
  - bar.0.0.1
  (version 0.0.1)
