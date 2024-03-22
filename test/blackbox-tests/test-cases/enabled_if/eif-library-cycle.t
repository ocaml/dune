Test cycles in enabled_if field of libraries

  $ cat > dune-project << EOF
  > (lang dune 3.15)
  > EOF

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
