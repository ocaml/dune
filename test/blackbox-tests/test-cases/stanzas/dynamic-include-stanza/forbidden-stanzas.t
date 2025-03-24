Some stanzas aren't allowed to be generated:

  $ cat >dune-project <<EOF
  > (lang dune 3.14)
  > (using dune_site 0.1)
  > (package (name foo))
  > EOF

  $ mkdir a b

  $ cat >b/dune <<EOF
  > (dynamic_include ../a/dune.inc)
  > (rule (with-stdout-to x (echo "")))
  > EOF

  $ cat >a/dune <<EOF
  > (copy_files ../dune.inc)
  > EOF

  $ runtest() {
  > cat >dune.inc
  > dune build b/x
  > }

  $ runtest <<EOF
  > (library (name foo))
  > EOF
  File "_build/default/a/dune.inc", line 1, characters 0-20:
  1 | (library (name foo))
      ^^^^^^^^^^^^^^^^^^^^
  Error: This stanza cannot be generated dynamically
  [1]

  $ runtest <<EOF
  > (install
  >  (section bin)
  >  (files foo as bar))
  > EOF
  File "_build/default/a/dune.inc", line 2, characters 10-13:
  2 |  (section bin)
                ^^^
  Error: binary section cannot be generated dynamically
  [1]

  $ runtest <<EOF
  > (executable
  >  (public_name foo))
  > EOF
  File "_build/default/a/dune.inc", lines 1-2, characters 0-31:
  1 | (executable
  2 |  (public_name foo))
  Error: This stanza cannot be generated dynamically
  [1]

  $ runtest <<EOF
  > (plugin
  >  (libraries)
  >  (name foo)
  >  (site (foo bar)))
  > EOF
  File "_build/default/a/dune.inc", line 4, characters 7-16:
  4 |  (site (foo bar)))
             ^^^^^^^^^
  Error: This stanza cannot be generated dynamically
  [1]

  $ runtest <<EOF
  > (deprecated_library_name
  >  (old_public_name foo)
  >  (new_public_name y))
  > EOF
  File "_build/default/a/dune.inc", lines 1-3, characters 0-69:
  1 | (deprecated_library_name
  2 |  (old_public_name foo)
  3 |  (new_public_name y))
  Error: This stanza cannot be generated dynamically
  [1]
