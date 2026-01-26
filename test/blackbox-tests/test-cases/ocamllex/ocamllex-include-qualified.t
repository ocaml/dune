  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF
  $ mkdir -p lib/bar
  $ cat > lib/dune <<EOF
  > (include_subdirs qualified)
  > (library (name foo))
  > EOF

  $ cat > lib/foo.ml <<EOF
  > let x = Bar.Lexer.lex
  > EOF

  $ cat > lib/bar/dune <<EOF
  > (ocamllex lexer)
  > EOF

  $ cat > lib/bar/lexer.mll <<EOF
  > {
  > }
  > rule lex = parse
  >   | _   { true  }
  >   | eof { false }
  > EOF

  $ dune build
