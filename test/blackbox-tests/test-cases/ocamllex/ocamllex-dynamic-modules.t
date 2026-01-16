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

  $ cat >foo.ml <<EOF
  > let x = Mod.lex
  > EOF
  $ cat >dune <<EOF
  > (ocamllex (:include gen/lst))
  > (library (name foo))
  > EOF

Building under dune 3.22 throws an error

  $ dune build foo.cma
  File "dune", line 1, characters 10-28:
  1 | (ocamllex (:include gen/lst))
                ^^^^^^^^^^^^^^^^^^
  Error: the ability to specify non-constant module lists is only available
  since version 3.22 of the dune language. Please update your dune-project file
  to have (lang dune 3.22).
  [1]

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > EOF
  $ dune build foo.cma

`%{read-lines:..}` also works

  $ dune clean
  $ cat >dune <<EOF
  > (ocamllex
  >  (modules (:include gen/lst)))
  > (library (name foo))
  > EOF

  $ dune build foo.cma
