Sandboxed actions see the sandboxed project root.

  $ mkdir -p proj/sub
  $ cat > proj/dune-project <<'EOF'
  > (lang dune 3.23)
  > EOF
  $ cat > proj/sub/dune <<'EOF'
  > (rule
  >  (target root.txt)
  >  (deps %{project_root}/dune-project)
  >  (action
  >   (with-stdout-to %{target}
  >    (bash "cat $DUNE_PROJECT_ROOT/dune-project > root.txt"))))
  > EOF
  $ dune build --root proj sub/root.txt
  Entering directory 'proj'
  Leaving directory 'proj'
  $ cat proj/_build/default/sub/root.txt
  (lang dune 3.23)

  $ dune build proj/sub/root.txt

  $ cat _build/default/proj/sub/root.txt
  (lang dune 3.23)
