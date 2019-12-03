Generation of opam files is attached to @all
--------------------------------------------
Reproduction case for #2927

  $ mkdir attached-to-all
  $ cat >attached-to-all/dune-project <<EOF
  > (lang dune 2.0)
  > (generate_opam_files true)
  > (package (name foo))
  > EOF

  $ cd attached-to-all && dune build

  $ cat attached-to-all/foo.opam
  cat: attached-to-all/foo.opam: No such file or directory
  [1]
