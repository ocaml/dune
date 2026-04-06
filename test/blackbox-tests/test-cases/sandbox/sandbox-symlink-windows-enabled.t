When symlinks are available on Windows (Developer Mode enabled), symlink
sandboxing should work correctly.

  $ make_dune_project 3.0

  $ echo hello > input

  $ cat > dune <<EOF
  > (rule
  >  (target a)
  >  (deps input (sandbox always))
  >  (action (copy input a)))
  > EOF

  $ dune build a --sandbox symlink

  $ cat _build/default/a
  hello
