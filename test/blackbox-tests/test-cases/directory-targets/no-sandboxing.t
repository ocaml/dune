Tests for directory targets that are produced by unsandboxed rule

  $ make_directory_targets_project 3.4

Build directory target from the command line without sandboxing

  $ cat > dune <<EOF
  > (rule
  >  (targets (dir output))
  >  (action (system "mkdir output; echo x > output/x; echo y > output/y")))
  > EOF

  $ dune build output/x
  $ cat _build/default/output/x
  x
  $ cat _build/default/output/y
  y

We ask to build a file that doesn't exist inside the directory:

  $ dune build output/fake
  File "dune", lines 1-3, characters 0-102:
  1 | (rule
  2 |  (targets (dir output))
  3 |  (action (system "mkdir output; echo x > output/x; echo y > output/y")))
  Error: This rule defines a directory target "output" that matches the
  requested path "output/fake" but the rule's action didn't produce it
  [1]

When we fail to create the directory, dune complains:

  $ cat > dune <<EOF
  > (rule
  >  (targets (dir output))
  >  (action (system "true")))
  > EOF

  $ dune build output/
  File "dune", lines 1-3, characters 0-56:
  1 | (rule
  2 |  (targets (dir output))
  3 |  (action (system "true")))
  Error: Rule failed to produce directory "output"
  [1]

Check that Dune clears stale files from directory targets.

  $ write_directory_target_contents_rules "src_a src_b src_c"

  $ echo a > src_a
  $ echo b > src_b
  $ echo c > src_c
  $ dune build contents
  running
  running
Directory target whose name conflicts with an internal directory used by Dune.

  $ cat > dune <<EOF
  > (rule
  >   (targets (dir .dune))
  >   (action (bash "mkdir .dune; echo hello > .dune/hello")))
  > EOF

  $ dune build .dune/hello
  File "dune", lines 1-3, characters 0-88:
  1 | (rule
  2 |   (targets (dir .dune))
  3 |   (action (bash "mkdir .dune; echo hello > .dune/hello")))
  Error: This rule defines a target ".dune" whose name conflicts with an
  internal directory used by Dune. Please use a different name.
  [1]

