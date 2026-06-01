Feeds an action's stdin from a file with `with-stdin-from`.

  $ make_dune_project 2.0

  $ cat > dune << EOF
  > (rule (with-stdin-from input (with-stdout-to output (run cat))))
  > EOF

  $ echo "Hello, Dune!" > input

  $ dune build --root . @all
  $ cat _build/default/output
  Hello, Dune!
