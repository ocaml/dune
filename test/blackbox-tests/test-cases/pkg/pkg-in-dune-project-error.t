Test that putting (pkg enabled) in dune-project instead of dune-workspace
gives a helpful error message.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (pkg enabled)
  > EOF

  $ dune build
  File "dune-project", line 2, characters 0-13:
  2 | (pkg enabled)
      ^^^^^^^^^^^^^
  Error: The (pkg ...) configuration is only valid in dune-workspace, not a
  dune-project.
  Hint: Add this configuration to your dune-workspace file (create one in your
  workspace root if you don't have one).
  [1]
