Demonstrate a dependency cycle between library modules field and a rule that
generates them dynamically in the same directory

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (with-stdout-to foo (echo "m_%{lib-available:bar}")))
  > (library
  >  (name foo)
  >  (modules %{read:foo}))
  > EOF

  $ cat > foo.ml << EOF
  > let x = "hello"
  > EOF

  $ dune build
  Error: Dependency cycle between:
     (modules) field at dune:3
  -> %{read:foo} at dune:5
  -> (modules) field at dune:3
  [1]

Rule in another dir

  $ mkdir gen
  $ cat > gen/dune << EOF
  > (rule
  >  (with-stdout-to foo (echo "m_%{lib-available:bar}")))
  > EOF
  $ touch m_false.ml

  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (modules %{read:gen/foo}))
  > EOF
  $ dune build


