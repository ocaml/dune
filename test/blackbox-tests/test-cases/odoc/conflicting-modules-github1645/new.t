We create two libraries `l.one` and `l.two` with a conflicting module.
They build fine, are not co-linkable, but documentation should be able to be
built. See #1645.

  $ cat > dune-project << EOF
  > (lang dune 1.0)
  > (package (name l))
  > EOF

  $ mkdir one
  $ cat > one/dune << EOF
  > (library
  >  (name l_one)
  >  (public_name l.one)
  >  (wrapped false))
  > EOF
  $ touch one/module.ml

  $ mkdir two
  $ cat > two/dune << EOF
  > (library
  >  (name l_two)
  >  (public_name l.two)
  >  (wrapped false))
  > EOF
  $ touch two/module.ml

  $ dune build @install
  $ dune build @doc-new
  File "Module":
  Ambiguous lookup. Possible files: Module
  Module
  File "Module":
  Ambiguous lookup. Possible files: Module
  Module
