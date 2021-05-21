Test when sites name which are ocaml keyword
---------------------------------

  $ cat >dune-project <<EOF
  > (lang dune 2.9)
  > (using dune_site 0.1)
  > (package
  >  (name my-package)
  >  (sites (lib include)))

  $ cat >dune <<EOF
  > (library (name lib) (libraries dune-site dune-site.plugins))
  > 
  > (generate_sites_module
  >  (module sites)
  >  (plugins (my-package include)))


  $ dune build
  File "Sites.ml", line 2, characters 8-15:
  2 |     let include = Dune_site.Private_.Helpers.site
              ^^^^^^^
  Error: Syntax error: 'end' expected
  File "Sites.ml", line 1, characters 15-21:
  1 | module Sites = struct
                     ^^^^^^
    This 'struct' might be unmatched
  [1]
