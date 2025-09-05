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

  $ dune build @runtest-a @runtest-b
  a
  b

Checking interaction with enabled_if

  $ cat > dune << EOF
  > (tests
  >  (names a b)
  >  (enabled_if false))
  > EOF

  $ dune build @a @b
  Error: Alias "a" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  Hint: did you mean all?
  Error: Alias "b" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  [1]
