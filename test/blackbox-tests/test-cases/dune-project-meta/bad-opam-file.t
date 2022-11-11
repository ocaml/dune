Fatal error with opam file that is not listed in the dune-project file:

  $ cat >dune-project <<EOF
  > (lang dune 1.10)
  > (version 1.0.0)
  > (generate_opam_files true)
  > (package (name bar))
  > EOF

  $ echo "cannot parse me" > foo.opam
  $ dune build @install
  File "foo.opam", line 1, characters 0-0:
  Error: This opam file doesn't have a corresponding (package ...) stanza in
  the dune-project file. Since you have at least one other (package ...) stanza
  in your dune-project file, you must a (package ...) stanza for each opam
  package in your project.
  [1]
