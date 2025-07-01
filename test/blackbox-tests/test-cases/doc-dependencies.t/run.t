This project contains an (empty) package, which has some doc dependencies:

  $ grep documentation -A 4 dune-project
   (doc_depends
    (packages
     (ppx_derivers (>= 1.0))
      brr)))

Doc dependencies are explained here: https://ocaml.github.io/odoc/odoc/odoc_for_authors.html#config-file

They need to be in the x-extra-doc-deps field of the opam file (to be able to
generate the opam universe) and installed in the odoc-config.sexp file (so that
the driver knows what to make available to reference). Let's check that.

Generate it's opam file
  $ dune build foo.opam

and check that the x-extra-doc-deps has been generated successfully
  $ grep extra-doc-deps foo.opam -A 3
  x-extra-doc-deps: [
    "ppx_derivers" {>= "1.0"}
    "brr"
  ]

The normal deps should not be modified
  $ grep depends foo.opam -A 3
  depends: [
    "dune" {>= "3.18"}
    "odoc" {with-doc}
  ]

Now, let's check that it would install the correct `odoc-config.sexp` file

  $ dune build @install

  $ cat _build/install/default/doc/foo/odoc-config.sexp
  (packages ppx_derivers brr)
