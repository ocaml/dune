Testing the behaviour of dune describe opam-files

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (generate_opam_files)
  > (package
  >  (name a)
  >  (allow_empty))
  > (package
  >  (name b)
  >  (allow_empty))
  > (package
  >  (name c)
  >  (allow_empty))
  > EOF

  $ dune describe opam-files --files
  a.opam
  b.opam
  c.opam

  $ cat >> dune-project <<EOF
  > (opam_file_location inside_opam_directory)
  > EOF
  $ dune describe opam-files --files
  opam/a.opam
  opam/b.opam
  opam/c.opam
