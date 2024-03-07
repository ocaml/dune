When a package fails to resolve, it shouldn't prevent rules from loading in a
directory.

Github issue #8630

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (using dune_site 0.1)
  > (package (name a))
  > EOF

  $ cat >dune <<EOF
  > (install
  >  (section (site (foobarpkg baz)))
  >  (files foo))
  > (rule (with-stdout-to foo (echo bar)))
  > EOF

  $ dune build foo
