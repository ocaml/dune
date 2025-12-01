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
  Error: The (pkg ...) stanza is only valid in dune-workspace files.
  Hint: Move this stanza to your dune-workspace file. If you don't have one,
                   create one in your workspace root.
  [1]
