Test when sites name which are ocaml keyword
---------------------------------

  $ cat >dune-project <<EOF
  > (lang dune 2.9)
  > (using dune_site 0.1)
  > (package
  >  (name my-package)
  >  (sites (lib include)))
  > EOF

  $ cat >dune <<EOF
  > (library (name lib) (libraries dune-site dune-site.plugins))
  > 
  > (generate_sites_module
  >  (module sites)
  >  (plugins (my-package include)))
  > EOF

  $ dune build
