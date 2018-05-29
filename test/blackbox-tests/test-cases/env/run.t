  $ dune printenv .
  (
   (flags (-w -40 -plop))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
  )
  $ dune printenv src
  (
   (flags (-w -40 -plop -truc))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
  )
  $ dune printenv bin
  (
   (flags (-machin))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
  )
  $ dune printenv vendor
  (
   (flags (-w -40 -plop))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
  )
  $ dune printenv vendor/a
  (
   (flags (-w -40 -bidule))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
  )
  $ dune printenv vendor/a/src
  (
   (flags (-w -40 -bidule -pouet))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
  )
