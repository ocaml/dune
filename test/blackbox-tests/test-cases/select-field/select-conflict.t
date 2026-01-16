We test the `(select)` field of the `(libraries)` field in the presence of
a conflicting rule

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ cat > dune <<EOF
  > (ocamllex (modules lexer))
  > (library
  >  (name foo)
  >  (libraries
  >   (select lexer.ml from
  >    (unix -> lexer.unix.ml)
  >    (!unix -> lexer.nounix.ml))))
  > EOF
  $ cat > lexer.mll <<EOF
  > {
  > }
  > rule lex = parse
  >   | _   { true  }
  >   | eof { false }
  > EOF

  $ cat > lexer.unix.ml <<EOF
  > let () = print_endline "Test: Unix was found!"
  > EOF
  $ cat > lexer.nounix.ml <<EOF
  > let () = print_endline "Test: Unix was not found!"
  > EOF

  $ dune build foo.cma
  Error: Multiple rules generated for _build/default/lexer.ml:
  - dune:1
  - dune:5
  [1]

