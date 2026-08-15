Test disabled install stanza in multi-context builds (github issue #15825).

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > (package
  >  (name repro)
  >  (allow_empty))
  > EOF

  $ cat > dune-workspace << EOF
  > (lang dune 3.21)
  > (context default)
  > (context
  >  (default
  >   (name other)))
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (enabled_if (= %{context_name} default))
  >  (target x)
  >  (action
  >   (write-file %{target} x)))
  > 
  > (install
  >  (section lib)
  >  (package repro)
  >  (enabled_if (= %{context_name} default))
  >  (files
  >   (x as c/x)))
  > EOF

  $ dune build
  Error: No rule found for x (context other)
  -> required by alias all (context other)
  -> required by alias default (context other)
  [1]

  $ cat _build/default/x
  x

  $ test -f _build/other/x
  [1]
