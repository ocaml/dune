Show an edge case of `(include_subdirs ..)` and ocamllex / menhir

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > (using menhir 3.0)
  > EOF

  $ mkdir -p gen

We define rules that create files (in the syntax expected by `modules`) that
each contain a single module name:

  $ cat >gen/dune <<EOF
  > (rule (with-stdout-to lst (echo my_parser)))
  > EOF

We add a `(menhir ..)` stanza in the group root dune file

  $ mkdir -p src/a
  $ cat > src/dune << EOF
  > (include_subdirs unqualified)
  > (library (name foo))
  > (ocamllex lexer)
  > EOF
  $ cat > src/a/dune << EOF
  > (menhir
  >  (modules %{read-lines:../gen/lst})
  >  (flags --dump))
  > EOF

  $ cat >src/lexer.mll  <<EOF
  > {
  > }
  > rule lex = parse
  >   | _   { true  }
  >   | eof { false }
  > EOF
  $ cat >src/a/my_parser.mly <<'EOF'
  > %token EOF
  > %start main
  > %type <unit> main
  > %%
  > main:
  >   | EOF { () }
  > EOF

Show that the menhir stanza must live next to the source

  $ dune build
  File "src/a/dune", line 2, characters 10-34:
  2 |  (modules %{read-lines:../gen/lst})
                ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]
