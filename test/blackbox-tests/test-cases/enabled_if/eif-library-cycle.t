Test cycles in enabled_if field of libraries

  $ make_dune_project 3.15

  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (enabled_if %{read:foo}))
  > (rule (with-stdout-to foo (echo true)))
  > EOF

  $ dune build
  Error: Dependency cycle between:
     library "foo" in _build/default
  -> %{read:foo} at dune:3
  -> library "foo" in _build/default
  [1]
