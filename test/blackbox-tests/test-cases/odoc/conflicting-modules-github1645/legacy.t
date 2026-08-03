We create two libraries `l.one` and `l.two` with a conflicting module.
They build fine, are not co-linkable, but documentation should be able to be
built. See #1645.

  $ make_dune_project_with_package 1.0 l

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
  $ dune build @doc
  Error: Multiple rules generated for _build/default/_doc/_html/l/Module:
  - <internal location>
  - <internal location>
  -> required by alias doc
  [1]
