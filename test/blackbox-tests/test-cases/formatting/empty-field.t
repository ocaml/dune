  $ cat > dune-project << EOF
  > (lang dune 3.5)
  > (formatting)
  > EOF

  $ dune build @fmt
