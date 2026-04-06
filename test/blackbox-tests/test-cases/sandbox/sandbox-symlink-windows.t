Symlink sandboxing requires Developer Mode on Windows. Without it,
attempting to use it should produce a user-facing error. We force symlinks
off via the config override to test this regardless of the host configuration.

  $ export DUNE_CONFIG__SYMLINKS_AVAILABLE=disabled

  $ make_dune_project 3.0

  $ echo hello > input

  $ cat > dune <<EOF
  > (rule
  >  (target a)
  >  (deps input (sandbox always))
  >  (action (copy input a)))
  > EOF

  $ dune build a --sandbox symlink
  File ".dune/_unknown_", line 1, characters 0-0:
  Error: Sandboxing mode symlink is not supported on this system. On Windows,
  enable Developer Mode in Windows Settings > Privacy & Security > For
  Developers, or use copy or hardlink mode instead.
  [1]
