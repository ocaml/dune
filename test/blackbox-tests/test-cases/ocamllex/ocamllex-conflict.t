Test conflicting modules where one is an `(ocamllex ..)` generated module

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

The unit `mod.mll` is present in the working tree, `lib.ml` uses it:

  $ cat > mod.ml <<EOF
  > let lex _ = true
  > EOF

  $ cat > mod.mll <<EOF
  > {
  > }
  > rule lex = parse
  >   | _   { true  }
  >   | eof { false }
  > EOF
  $ cat > mod.mli <<EOF
  > val lex: Lexing.lexbuf -> bool
  > EOF

  $ cat >foo.ml <<EOF
  > let x = Mod.lex
  > EOF
  $ cat >dune <<EOF
  > (library (name foo))
  > EOF

dune build doesn't pick up `mod.mll`

  $ dune build foo.cma

Adding `(ocamllex mod)` shows multiple rules generated

  $ cat >dune <<EOF
  > (ocamllex mod)
  > (library (name foo))
  > EOF

  $ dune build foo.cma
  Error: Multiple rules generated for _build/default/mod.ml:
  - dune:1
  - file present in source tree
  Hint: rm -f mod.ml
  [1]
