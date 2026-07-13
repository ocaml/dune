Directory targets and action [chdir] paths are recreated after replay clears
the target.

  $ make_dune_project_with_extension 3.23 directory-targets 0.1

  $ cat > dune <<'EOF'
  > (rule
  >  (targets (dir output-dir))
  >  (action
  >   (progn
  >    (run mkdir -p output-dir)
  >    (chdir output-dir (run sh -c "echo directory-target > value")))))
  > EOF

  $ dune shell --sandbox=copy _build/default/output-dir -- sh -c '
  > "$DUNE_SHELL/dune-run"
  > printf "directory-target: "; cat output-dir/value
  > '
  directory-target: directory-target
