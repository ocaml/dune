Make sure Dune generates a correct odoc configuration file that includes
packages which are marked `with-doc`.

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >   without-with-doc
  >   (ppx_derivers (and (>= 1.0) :with-doc))
  >   (brr :with-doc)))
  > EOF

Dependencies for odoc are explained here:
https://ocaml.github.io/odoc/odoc/odoc_for_authors.html#config-file

Now, let's check that it would install the correct `odoc-config.sexp` file:

  $ dune build @install
  $ cat _build/install/default/doc/foo/odoc-config.sexp
  (packages ppx_derivers brr)
