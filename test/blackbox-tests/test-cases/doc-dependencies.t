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

Now let's check that specifying doc dependencies outside of a package stanza
generates a warning:

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > (generate_opam_files true)
  > (documentation
  >  (depends
  >   (ppx_derivers (>= 1.0))
  >    brr)
  >  (url "url"))
  > (package
  >  (name foo)
  >  (allow_empty))
  > EOF

  $ dune build foo.opam
  File "dune-project", lines 4-6, characters 1-43:
  4 |  (depends
  5 |   (ppx_derivers (>= 1.0))
  6 |    brr)
  Warning: The depends field of the documentation stanza can only be non-empty
  when the documentation stanza is inside a package stanza.
