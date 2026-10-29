This test makes sure that whenever we evaluate a glob inside a directory target
that matches nothing, we still copy the directory and make it empty.

  $ make_directory_targets_project 3.0

  $ cat > dune <<EOF
  > (rule
  >  (targets (dir output))
  >  (action (system "mkdir output && touch output/foo.txt")))
  > (rule
  >  (targets x)
  >  (deps (glob_files output/*.baz))
  >  (action (system "ls output/ && touch x")))
  > EOF

  $ DUNE_SANDBOX=copy dune build x

Globs in nested directories must stay rooted at the requested subtree when one
rule also produces sibling directory and file targets.

  $ cat > dune <<'EOF'
  > (rule
  >  (targets (dir left) (dir right) ordinary.txt)
  >  (deps value)
  >  (action
  >   (bash "mkdir -p left/nested right/nested; cat value > left/nested/shared.txt; echo right > right/nested/shared.txt; echo ordinary > ordinary.txt")))
  > (rule
  >  (target summary)
  >  (deps (glob_files left/nested/*.txt) (glob_files right/nested/*.txt))
  >  (action (with-stdout-to summary (run cat %{deps}))))
  > EOF
  $ echo left > value
  $ DUNE_SANDBOX=copy dune build summary
  $ cat _build/default/summary
  left
  right

  $ echo changed > value
  $ DUNE_SANDBOX=copy dune build summary
  $ cat _build/default/summary
  changed
  right
  $ dune build left/nested/shared.txt right/nested/shared.txt ordinary.txt
