
  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (using menhir 3.0)
  > EOF

  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (executable
  >  (name foo))
  > EOF

$ cat >ast.ml <<EOF
> type expr =
>   | Unit
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

  $ mkdir -p lang/sub

  $ cat >lang/dune <<EOF
  > (menhir (modules parser))
  > EOF

  $ cat >lang/ast.ml <<EOF
  > type expr =
  >   | Unit
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

Use `merge_into`

  $ cat >lang/sub/tokens.mly <<EOF
  > %token <char> TOKEN
  > %token EOF
  > %%
  > EOF
  $ cat >lang/sub/parser.mly <<EOF
  > %start <char list> main
  > %%
  > main:
  > | c = TOKEN EOF { [c] }
  > | c = TOKEN xs = main  { c :: xs }
  > EOF

  $ cat >lang/sub/dune <<EOF
  > (menhir (modules tokens parser) (merge_into both))
  > EOF

  $ dune build

