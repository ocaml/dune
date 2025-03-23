Tests stanzas produce aliases with the executable names

  $ cat > dune-project << EOF
  > (lang dune 3.17)
  > EOF

  $ cat > dune << EOF
  > (tests
  >  (names a b))
  > EOF

  $ cat > a.ml << EOF
  > let () = print_endline "a"
  > EOF

  $ cat > b.ml << EOF
  > let () = print_endline "b"
  > EOF

  $ dune build @a @b
  a
  b
