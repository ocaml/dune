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
  $ docs=_build/default/_doc_new/html/docs/local/l
  $ dune build \
  >   "$docs/one/Module/index.html" \
  >   "$docs/two/Module/index.html"
  File "Module":
  Ambiguous lookup. Possible files: Module
  Module
  File "Module":
  Ambiguous lookup. Possible files: Module
  Module
