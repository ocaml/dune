This test demonstrates the interaction between --ignore-promoted-rules and
internal promote rules (like generating opam files)

Reported in #8417

  $ cat >dune-project <<EOF
  > (lang dune 3.10)
  > (generate_opam_files true)
  > (package
  >  (name foo))
  > EOF

  $ dune build foo.opam
  $ echo foobar_extra >> foo.opam
  $ grep foobar_extra foo.opam
  foobar_extra

This should not modify the file now

  $ dune build --ignore-promoted-rules foo.opam
  $ grep extra foo.opam
  [1]
