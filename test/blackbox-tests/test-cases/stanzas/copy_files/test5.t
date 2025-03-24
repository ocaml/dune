Test (enabled_if ...)

  $ mkdir -p subdir
  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > EOF
  $ cat >subdir/dune <<EOF
  > (rule (with-stdout-to foo.txt (progn)))
  > EOF
  $ cat >dune <<EOF
  > (copy_files (enabled_if false) (files subdir/foo.txt))
  > EOF
  $ dune build
  $ ls _build/default | grep foo.txt
  [1]
  $ cat >dune <<EOF
  > (copy_files (enabled_if true) (files subdir/foo.txt))
  > EOF
  $ dune build
  $ ls _build/default | grep foo.txt
  foo.txt
