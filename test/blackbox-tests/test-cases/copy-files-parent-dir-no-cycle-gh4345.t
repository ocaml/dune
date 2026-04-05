This is a reproduction case from issue #4345.
This was a bug where using (copy_files ...) to depend on files from a parent
directory would cause in internal error in dune due to a dependency cycle. The
bug is now fixed, so this project should build without error.

  $ DIR="gh4345"
  $ mkdir $DIR && cd $DIR
  $ echo "(lang dune 2.8)" > dune-project
  $ mkdir lib
  $ touch lib.opam file lib/lib.ml
  $ cat >lib/dune <<EOF
  > (library (name lib) (public_name lib))
  > (copy_files (files ../file))
  > EOF
  $ dune build --root .
