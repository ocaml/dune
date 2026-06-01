Test expanding variables in `(promote (into ..))`

  $ make_dune_project 3.21
  $ mkdir -p a/b another
  $ cat > a/b/dune <<EOF
  > (rule
  >  (targets promoted)
  >  (mode (promote
  >   (into %{workspace_root}/another)
  >   (until-clean)))
  >  (action
  >   (with-stdout-to promoted (echo "Hello, world!"))))
  > EOF

  $ dune build a/b/promoted

  $ dune trace cat | jq 'select(.name == "promote") | .args'
  {
    "src": "_build/default/a/b/promoted",
    "dst": "another/promoted"
  }

  $ cat another/promoted
  Hello, world!
