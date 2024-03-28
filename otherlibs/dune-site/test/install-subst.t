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

  $ has_placeholder() {
  >   name=$1;
  >   if grep -q DUNE_PLACEHOLDER "$name"; then
  >     printf "%-30s has placeholders\n" "$name"
  >   else
  >     printf "%-30s has no placeholders\n" "$name"
  >   fi
  > }

The generated modules has placeholders.

  $ has_placeholder _build/default/S.ml
  _build/default/S.ml            has placeholders

  $ dune install a --prefix out/ --datadir /nonexistent/data

We expect no placeholders to be present after installation.

  $ find out -type f | sort | while read f ; do has_placeholder $f ; done
  out/lib/a/META                 has no placeholders
  out/lib/a/S.ml                 has no placeholders
  out/lib/a/a.a                  has no placeholders
  out/lib/a/a.cma                has no placeholders
  out/lib/a/a.cmi                has no placeholders
  out/lib/a/a.cmt                has no placeholders
  out/lib/a/a.cmx                has no placeholders
  out/lib/a/a.cmxa               has no placeholders
  out/lib/a/a.cmxs               has no placeholders
  out/lib/a/a.ml                 has no placeholders
  out/lib/a/a__S.cmi             has no placeholders
  out/lib/a/a__S.cmt             has no placeholders
  out/lib/a/a__S.cmx             has no placeholders
  out/lib/a/dune-package         has no placeholders
