Test the ability of `(modules ..)` to contain dynamic
forms such as `(:include)` and variables such as `"%{read-lines:}"` in the
ocamllex / ocamlyacc stanzas.

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ mkdir -p gen

We define a rule that creates a file (in sexp syntax, to be passed to
`(:include)`) containing a single name:

  $ cat >gen/dune <<EOF
  > (rule (with-stdout-to lst (echo mod)))
  > EOF

The unit `mod.mll` is present in the working tree, `lib.ml` uses it:

  $ cat > mod.mll <<EOF
  > {
  > }
  > rule lex = parse
  >   | _   { true  }
  >   | eof { false }
  > EOF

file `gen/lst`:

  $ cat >dune <<EOF
  > (ocamllex
  >  (modules (:include gen/lst)))
  > EOF

  $ dune b
  File "dune", line 2, characters 10-28:
  2 |  (modules (:include gen/lst)))
                ^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]

