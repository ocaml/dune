  $ dune printenv --profile default .
  (
   (flags (-w -40 -plop))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
  )
  $ dune printenv --profile default src
  (
   (flags (-w -40 -plop -truc))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
  )
  $ dune printenv --profile default bin
  (
   (flags (-machin))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
  )
  $ dune printenv --profile default vendor
  (
   (flags (-w -40 -plop))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
  )
  $ dune printenv --profile default vendor/a
  (
   (flags (-w -40 -bidule))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
  )
  $ dune printenv --profile default vendor/a/src
  (
   (flags (-w -40 -bidule -pouet))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
  )
