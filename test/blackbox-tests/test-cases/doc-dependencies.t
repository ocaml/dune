Make sure Dune generates a correct odoc configuration file that includes
packages which are marked `with-doc`.

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >   (ppx_derivers (and (>= 1.0) :with-doc))
  >   (brr :with-doc)))
  > EOF

Dependencies for odoc are explained here:
https://ocaml.github.io/odoc/odoc/odoc_for_authors.html#config-file

Generate its opam file:

  $ dune build foo.opam

The depends field should be modified accordingly:

  $ grep depends foo.opam -A 5
  depends: [
    "dune" {>= "3.22"}
    "ppx_derivers" {>= "1.0" & with-doc & post}
    "brr" {with-doc & post}
    "odoc" {with-doc & post}
  ]

Now, let's check that it would install the correct `odoc-config.sexp` file

  $ dune build @install

  $ cat _build/install/default/doc/foo/odoc-config.sexp
  (packages ppx_derivers brr)
