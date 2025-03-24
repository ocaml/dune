This checks that generated sites files correctly substituted with `dune install`.

See #10317.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (using dune_site 0.1)
  > (package
  >  (name a)
  >  (sites (share data)))
  > EOF

  $ cat > dune << EOF
  > (library
  >  (public_name a)
  >  (libraries dune-site))
  > (generate_sites_module
  >  (module s)
  >  (sites a))
  > EOF

  $ dune build

  $ check_placeholder() {
  >   name=$1;
  >   if grep -q DUNE_PLACEHOLDER "$name"; then
  >     echo placeholder found in $name
  >   fi
  > }

The generated module has placeholders.

  $ check_placeholder _build/default/S.ml
  placeholder found in _build/default/S.ml

  $ dune install a --prefix out/ --datadir /nonexistent/data

We expect no placeholders to be present after installation.

  $ find out -type f | sort | while read f ; do check_placeholder $f ; done
