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

  $ dune printenv --profile default run
  (
   (flags (-w -40))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
   (c_flags (-DTEST_C))
   (cxx_flags (-DTEST_CPP))
  )
  $ dune exec --profile default ./run/foo.exe
  (cd _build/default/run && ./foo.exe)
  DTEST_C defined.
  [1]
  $ dune exec --profile default ./run/bar.exe
  (cd _build/default/run && ./bar.exe)
  DTEST_CPP defined.
  [1]
