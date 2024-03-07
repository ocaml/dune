This shows a difference since #8518: we set up a project with a generated opam
file. If the resulting file is not present in the repository (or if it has been
renamed to just `opam` as in this example), we now fail.

A related issue happens when `pkg.opam` exists but is not up to date. The
instruction in the opam file would previously consider the original version but
will overwrite `pkg.opam`. Now it does not update the file.

  $ cat > dune-project << EOF
  > (lang dune 1.10)
  > (generate_opam_files)
  > (package
  >  (name pkg))
  > EOF

  $ touch opam

  $ dune build -p pkg @install
