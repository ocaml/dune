Subsetting packages to be built should also work when the OPAM files are in the
`opam/` subfolder.

For this we create a project with manually created `opam` files in the `opam/`
subfolder.

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (name mypkg)
  > (opam_file_location inside_opam_directory)
  > EOF
  $ mkdir opam
  $ cat > opam/mypkg.opam <<EOF
  > opam-version: 2.0
  > EOF

Building all packages should work:

  $ dune build

Building just this package should work:

  $ dune build -p mypkg
  Error: I don't know about package mypkg (passed through --only-packages)
  [1]
