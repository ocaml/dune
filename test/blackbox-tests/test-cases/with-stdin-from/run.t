  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat > dune << EOF
  > (rule (with-stdin-from input (with-stdout-to output (run cat))))
  > EOF

  $ echo "Hello, Dune!" > input

  $ dune build --root . @all
  $ cat _build/default/output
  Hello, Dune!
