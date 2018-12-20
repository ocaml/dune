  $ dune printenv --profile default .
  (
   (flags (-w -40))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
   (c_flags (":standard + in ."))
   (cxx_flags (":standard + in ."))
  )

  $ dune printenv --profile default src
  (
   (flags (-w -40))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
   (c_flags (":standard + in ." ":standard + in src"))
   (cxx_flags (":standard + in ." ":standard + in src"))
  )

  $ dune printenv --profile default bin
  (
   (flags (-w -40))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
   (c_flags ("in bin"))
   (cxx_flags ("in bin"))
  )
