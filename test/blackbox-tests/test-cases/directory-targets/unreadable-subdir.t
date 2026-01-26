Test that dune produces a clear error when a rule creates an unreadable
directory.

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (targets (dir output))
  >  (action (bash "mkdir -p output/subdir; echo content > output/subdir/file.txt; chmod -r output/subdir")))
  > EOF

  $ dune build output
  File "dune", lines 1-3, characters 0-135:
  1 | (rule
  2 |  (targets (dir output))
  3 |  (action (bash "mkdir -p output/subdir; echo content > output/subdir/file.txt; chmod -r output/subdir")))
  Error: Rule produced unreadable directory "output/subdir"
  Permission denied
  [1]

Restore permissions for cleanup:

  $ chmod -R +r _build 2>/dev/null; rm -rf _build
