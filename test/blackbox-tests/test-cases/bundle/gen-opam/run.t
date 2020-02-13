Bundled dependencies should be excluded from opam files

  $ cat >dune-project <<EOF
  > (lang dune 2.3)
  > (generate_opam_files true)
  > (package (name bundledlib))
  > (package
  >  (name bin)
  >  (depends (bundledlib :bundle)))
  > EOF

  $ mkdir bundledlib bin
  $ touch bundledlib/bundledlib.ml bin/bin.ml

  $ cat >bundledlib/dune <<EOF
  > (library (public_name bundledlib))
  > EOF

  $ cat >bin/dune <<EOF
  > (executable (package bin) (public_name bin) (libraries bundledlib))
  > EOF

  $ dune build @check
  $ sed -n '/^depends:/,/^\]/p' bin.opam
  depends: [
    "dune" {>= "2.3"}
  ]

