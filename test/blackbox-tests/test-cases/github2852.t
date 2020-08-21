This test makes sure that copy_files works from inside (include ..) It could
possibly fail if interpreted relative to an incorrect dir.

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF
  $ cat >dune.inc <<EOF
  > (copy_files# ../aux.ml)
  > EOF
  $ mkdir -p a
  $ cat >a/dune <<EOF
  > (include ../dune.inc)
  > (executable (name aux))
  > EOF
  $ cat > aux.ml <<EOF
  > print_endline "success"
  > EOF
  $ dune exec ./a/aux.exe
  success
