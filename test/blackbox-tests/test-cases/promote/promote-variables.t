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

  $ dune build a/b/promoted --verbose 2>&1 | grep "Promoting"
  Promoting "_build/default/a/b/promoted" to "another/promoted"
  $ cat another/promoted
  Hello, world!

