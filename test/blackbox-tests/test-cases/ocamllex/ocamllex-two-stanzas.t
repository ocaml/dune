Test building 2 ocamllex stanzas in the same group, by 2 different stanzas 
(library + executable)

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF
  $ cat > dune <<EOF
  > (library (name foo) (modules foo foo_lex))
  > (executable (name bar) (modules bar bar_lex))
  > (ocamllex (modules bar_lex foo_lex))
  > EOF
  $ cat > bar_lex.mll <<EOF
  > {
  > }
  > rule lex = parse
  >   | _   { true  }
  >   | eof { false }
  > EOF

  $ cat > foo_lex.mll <<EOF
  > {
  > }
  > rule lex = parse
  >   | _   { true  }
  >   | eof { false }
  > EOF

  $ cat > foo.ml<<EOF
  > let x = Foo_lex.lex
  > EOF

  $ cat > bar.ml<<EOF
  > let x = Bar_lex.lex
  > EOF

  $ dune build
