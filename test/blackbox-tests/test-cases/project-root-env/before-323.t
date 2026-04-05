DUNE_PROJECT_ROOT is not set before dune lang 3.23.

  $ mkdir -p proj/sub

  $ cat > proj/dune-project <<'EOF'
  > (lang dune 3.22)
  > EOF

  $ cat > proj/sub/dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (action
  >   (bash "test \"${DUNE_PROJECT_ROOT-unset}\" = unset && echo ok")))
  > EOF

  $ dune build @proj/sub/runtest
  ok
