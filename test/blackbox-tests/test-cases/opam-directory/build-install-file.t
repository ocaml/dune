Generating .install files when opam files are in opam/ dir

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (generate_opam_files true)
  > (opam_file_location inside_opam_directory)
  > (package
  >  (name foobar))
  > EOF

  $ dune build foobar.install
  Error: Don't know how to build foobar.install
  [1]
