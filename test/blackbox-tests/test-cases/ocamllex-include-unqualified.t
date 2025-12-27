
  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF
  $ mkdir -p lib/foo
  $ cat > lib/dune <<EOF
  > (include_subdirs unqualified)
  > (library (name foo))
  > EOF

  $ cat > lib/foo/dune <<EOF
  > (ocamllex lexer)
  > EOF
  $ cat > lib/foo.ml <<EOF
  > let x = Lexer.lex
  > EOF
  $ cat > lib/foo/lexer.mll <<EOF
  > {
  > }
  > rule lex = parse
  >   | _   { true  }
  >   | eof { false }
  > EOF
  $ dune build ./lib/foo/lexer.ml



