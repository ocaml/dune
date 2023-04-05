Generating .install files when opam files are in opam/ dir

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (generate_opam_files true)
  > (opam_file_location inside_opam_directory)
  > (package
  >  (allow_empty)
  >  (name foobar))
  > EOF

  $ dune build foobar.install
