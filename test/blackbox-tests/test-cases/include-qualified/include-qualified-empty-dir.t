Test `(include_subdirs qualified)` in the presence of invalid module name
directories that don't contain source files

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ cat > dune <<EOF
  > (include_subdirs qualified)
  > (library (name foo))
  > EOF
  $ mkdir bar
  $ echo hello > bar/some-data.txt

  $ mkdir bar-baz

The directory `bar-baz`, even though not a valid module name, doesn't have any
source files. The library should still compile.

  $ dune build foo.cma

