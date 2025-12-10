Test expanding variables in `(promote (into ..))`

  $ echo "(lang dune 3.21)" > dune-project
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

  $ dune build a/b/promoted --trace-file trace.json

  $ jq '.[] | select(.name == "promote") | .args' trace.json
  {
    "src": "_build/default/a/b/promoted",
    "dst": "another/promoted"
  }

  $ cat another/promoted
  Hello, world!
