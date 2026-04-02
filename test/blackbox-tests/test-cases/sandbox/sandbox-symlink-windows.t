Symlink sandboxing is not supported on Windows. Attempting to use it should
produce a user-facing error.

  $ make_dune_project 3.0

  $ cat > dune <<EOF
  > (rule
  >  (target a)
  >  (deps (sandbox always))
  >  (action (with-stdout-to a (echo "hello"))))
  > EOF

  $ dune build a --sandbox symlink
  File ".dune/_unknown_", line 1, characters 0-0:
  Error: Sandboxing mode symlink is not supported on Windows. Use copy or
  hardlink instead.
  [1]
