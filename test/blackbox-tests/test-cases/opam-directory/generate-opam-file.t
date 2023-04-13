Generate opam files in the opam/ sub directory

First the case when opam/ doesn't exist in the source

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (generate_opam_files true)
  > (opam_file_location inside_opam_directory)
  > (package
  >  (name foobar))
  > EOF

  $ dune build @check
  $ ls opam/foobar.opam
  opam/foobar.opam

Now we test the case whenever opam is a source directory

  $ rm opam/foobar.opam
  $ dune build @check
  $ ls opam/foobar.opam
  opam/foobar.opam

We forbid .opam files in the root directory:

  $ touch bar.opam
  $ dune build @check
  Error: When (opam_file_location inside_opam_directory) is set, all opam files
  must live in the opam/ subdirecotry. The following opam files must be moved:
  - bar.opam
  [1]
