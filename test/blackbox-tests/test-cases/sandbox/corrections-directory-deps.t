Corrections under a copied directory dependency are still promoted

  $ make_dune_project 3.23
  $ mkdir -p inner/sub
  $ echo original > inner/sub/b
  $ cat > dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (corrections produce)
  >  (deps (source_tree inner))
  >  (action (system "echo corrected > inner/sub/b.corrected")))
  > EOF

Copying is needed because we're writing into the copied dependency tree
  $ dune build @runtest --sandbox copy > /dev/null 2>&1 || echo failed
  failed
  $ dune promotion list
  inner/sub/b
