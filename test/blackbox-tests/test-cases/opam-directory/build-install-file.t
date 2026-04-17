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

Checked-in opam files in opam/ should stay the source of truth for release
style builds.

  $ rm -rf _build opam
  $ cat >dune-project <<EOF
  > (lang dune 3.23)
  > (generate_opam_files true)
  > (opam_file_location inside_opam_directory)
  > (package
  >  (allow_empty)
  >  (name foobar))
  > EOF

  $ mkdir opam
  $ cat >opam/foobar.opam <<EOF
  > version: "0.1"
  > # SOURCE COPY
  > opam-version: "2.0"
  > EOF

  $ dune build -p foobar @install _build/install/default/lib/foobar/opam
  $ grep '^# SOURCE COPY$' _build/install/default/lib/foobar/opam
  # SOURCE COPY
  $ grep '^# SOURCE COPY$' opam/foobar.opam
  # SOURCE COPY
