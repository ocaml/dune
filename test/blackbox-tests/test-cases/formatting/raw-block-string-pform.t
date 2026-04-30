Pform-like text in raw block strings should be literal, not expanded.

  $ cat > dune-project << EOF
  > (lang dune 3.17)
  > EOF

  $ cat > dune << 'EOF'
  > (rule
  >  (alias default)
  >  (action
  >   (echo "\> %{name}
  > )))
  > EOF

  $ dune build 2>&1
  %{name}

In escaped block strings, pforms are expanded as usual.

  $ cat > dune << 'EOF'
  > (rule
  >  (alias default)
  >  (action
  >   (echo "\| %{project_root}
  > )))
  > EOF

  $ dune build 2>&1
  .
