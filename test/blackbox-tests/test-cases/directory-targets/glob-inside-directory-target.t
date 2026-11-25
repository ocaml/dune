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

A recursive glob rooted at a directory target should include generated files
from its subdirectories as well as its root.

  $ cat > dune <<'EOF'
  > (rule
  >  (target (dir tree))
  >  (action
  >   (system "\| mkdir -p tree/sub
  >           "\| echo root > tree/root.txt
  >           "\| echo nested > tree/sub/nested.txt
  >   )))
  > (rule
  >  (target report)
  >  (deps (glob_files_rec tree/*.txt) (sandbox always))
  >  (action (with-stdout-to report (run cat %{deps}))))
  > EOF
  $ dune build --sandbox=copy report
  $ test -f _build/default/tree/sub/nested.txt

CR: The recursive glob only discovers subdirectories in the source tree, so
it silently omits the generated nested file.

  $ cat _build/default/report
  root

A glob whose directory component is actually a generated file should not
raise an internal error.

  $ cat >> dune <<'EOF'
  > (rule
  >  (alias glob-in-file)
  >  (deps (glob_files tree/root.txt/*))
  >  (action (echo %{deps})))
  > EOF

  $ dune build @glob-in-file > build.log 2>&1
  [1]
  $ grep -F 'Error:' build.log
  Error: "tree/root.txt" is not a directory.
