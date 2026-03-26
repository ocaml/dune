Anonymous actions can discover their project root from DUNE_PROJECT_ROOT.

  $ mkdir -p proj/sub
  $ cat > proj/dune-project <<'EOF'
  > (lang dune 3.23)
  > EOF
  $ cat > proj/sub/dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (action
  >   (bash "test \"$DUNE_PROJECT_ROOT\" = \"$(dirname \"$PWD\")\" && echo ok")))
  > EOF

  $ dune build --root proj @sub/runtest
  Entering directory 'proj'
  ok
  Leaving directory 'proj'

  $ dune build @proj/sub/runtest
  ok
