  $ dune printenv --profile default .
  (
   (flags (-w -40 ":standard + in ."))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
   (c_flags ())
   (cxx_flags ())
  )
  $ dune printenv --profile default src
  (
   (flags (-w -40 ":standard + in ." ":standard + in src"))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
   (c_flags ())
   (cxx_flags ())
  )
  $ dune printenv --profile default bin
  (
   (flags ("in bin"))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
   (c_flags ())
   (cxx_flags ())
  )
  $ dune printenv --profile default vendor
  (
   (flags (-w -40 ":standard + in ."))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
   (c_flags ())
   (cxx_flags ())
  )

Vendored project without env customization, the global default should
apply:

  $ export STANDARD_C_FLAGS=`ocamlc -config | grep ocamlc_cflags | sed 's/ocamlc_cflags: //'`
  $ export STANDARD_CXX_FLAGS=`ocamlc -config | grep ocamlc_cflags | sed 's/ocamlc_cflags: //' | sed 's/-std=[^ ]* //' | sed 's/-std=[^$]*//'`
  $ dune printenv --profile default vendor/without-env-customization  | tr '\n' ' '  | sed 's/ \+/ /g' | sed "s/$STANDARD_C_FLAGS/STANDARD_C_FLAGS/" | sed "s/$STANDARD_CXX_FLAGS/STANDARD_CXX_FLAGS/"
  ( (flags (-w -40)) (ocamlc_flags (-g)) (ocamlopt_flags (-g)) (c_flags (STANDARD_C_FLAGS)) (cxx_flags (STANDARD_CXX_FLAGS)) ) 

Vendored project with env customization, the global default +
customization of vendored project should apply:

  $ dune printenv --profile default vendor/with-env-customization
  (
   (flags (-w -40 ":standard + in vendor/with-env-customization"))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
   (c_flags ())
   (cxx_flags ())
  )
  $ dune printenv --profile default vendor/with-env-customization/src
  (
   (flags ("in vendor/with-env-customization/src"))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
   (c_flags ())
   (cxx_flags ())
  )

