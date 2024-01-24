Show that copy_files operates on the build folder, copying over e.g. .re.ml files

  $ mkdir -p subdir
  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF
  $ cat >subdir/dune <<EOF
  > (library (name foo))
  > EOF
  $ cat >subdir/foo.re <<EOF
  > let t = 1
  > EOF
  $ cat >dune <<EOF
  > (copy_files (enabled_if true) (files subdir/*.ml))
  > EOF
  $ dune build
  $ ls _build/default | grep foo.re.ml
  foo.re.ml
