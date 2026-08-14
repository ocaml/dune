The dune-site plugin stanza is namespaced starting in Dune 3.26.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using dune_site 0.1)
  > (package (name foo))
  > EOF
  $ cat > dune <<'EOF'
  > (dune_site.plugin
  >  (name foo)
  >  (libraries foo)
  >  (site (foo plugins)))
  > EOF

The namespaced stanza is not available in Dune language 3.25.

  $ dune build @install
  File "dune", lines 1-4, characters 0-69:
  1 | (dune_site.plugin
  2 |  (name foo)
  3 |  (libraries foo)
  4 |  (site (foo plugins)))
  Error: 'dune_site.plugin' is only available since version 3.26 of the dune
  language. Please update your dune-project file to have (lang dune 3.26).
  [1]

The original spelling still parses as a site plugin in Dune language 3.25.
Site validation happens later.

  $ cat > dune <<'EOF'
  > (plugin
  >  (name foo)
  >  (libraries foo)
  >  (site (foo plugins)))
  > EOF
  $ dune build @install
  File "dune", line 4, characters 7-20:
  4 |  (site (foo plugins)))
             ^^^^^^^^^^^^^
  Error: Package foo doesn't define a site plugins
  [1]

Starting with Dune language 3.26, use the namespaced spelling instead.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (using dune_site 0.1)
  > (package (name foo))
  > EOF
  $ cat > dune <<'EOF'
  > (dune_site.plugin
  >  (name foo)
  >  (libraries foo)
  >  (site (foo plugins)))
  > EOF
  $ dune build @install
  File "dune", line 4, characters 7-20:
  4 |  (site (foo plugins)))
             ^^^^^^^^^^^^^
  Error: Package foo doesn't define a site plugins
  [1]
