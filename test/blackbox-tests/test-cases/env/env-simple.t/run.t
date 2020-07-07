  $ alias dune_printenv='dune printenv --profile default --field flags'

  $ dune_printenv .
  (flags
   (-w -40 ":standard + in ."))

  $ dune_printenv src
  (flags
   (-w -40 ":standard + in ." ":standard + in src"))

  $ dune_printenv bin
  (flags ("in bin"))

  $ dune_printenv vendor
  (flags
   (-w -40 ":standard + in ."))

Vendored project without env customization, the global default should
apply:

  $ dune_printenv vendor/without-env-customization
  (flags
   (-w -40))

Vendored project with env customization, the global default +
customization of vendored project should apply:

  $ dune_printenv vendor/with-env-customization
  (flags
   (-w -40 ":standard + in vendor/with-env-customization"))

  $ dune_printenv vendor/with-env-customization/src
  (flags ("in vendor/with-env-customization/src"))

