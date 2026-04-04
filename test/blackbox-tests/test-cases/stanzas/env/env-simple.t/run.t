  $ printenv() {
  > dune printenv --profile default --field flags $@
  > }

  $ printenv .
  (flags
   (-w -40 ":standard + in ."))

  $ printenv src
  (flags
   (-w -40 ":standard + in ." ":standard + in src"))

  $ printenv bin
  (flags ("in bin"))

  $ printenv vendor
  (flags
   (-w -40 ":standard + in ."))

Vendored project without env customization, the global default should
apply:

  $ printenv vendor/without-env-customization
  (flags
   (-w -40))

Vendored project with env customization, the global default +
customization of vendored project should apply:

  $ printenv vendor/with-env-customization
  (flags
   (-w -40 ":standard + in vendor/with-env-customization"))

  $ printenv vendor/with-env-customization/src
  (flags ("in vendor/with-env-customization/src"))

