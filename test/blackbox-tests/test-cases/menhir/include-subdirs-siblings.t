
  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (using menhir 3.0)
  > EOF

  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (executable
  >  (name foo))
  > EOF

  $ cat >foo.ml <<EOF
  > let () =
  >   assert
  >     (Lang.Parser.expr
  >       (fun _ -> Lang.Parser.EOF)
  >       (Lexing.from_string "")
  >       =
  >       Lang.Ast.Unit)
  > EOF

  $ mkdir -p lang

  $ cat >lang/ast.ml <<EOF
  > type expr =
  >   | Unit
  > EOF
  $ cat >lang/dune <<EOF
  > (menhir
  >  (modules parser))
  > EOF

  $ cat >lang/parser.mly <<EOF
  > %token EOF
  > %start <Ast.expr> expr
  > %%
  > expr:
  > | EOF { Ast.Unit }
  > EOF

Menhir parsers in qualified subdirectories should be able to refer to sibling modules:

  $ dune build
