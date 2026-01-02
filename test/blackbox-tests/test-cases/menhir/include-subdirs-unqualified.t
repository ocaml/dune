Exercise the `(menhir ..)` stanza with `(include_subdirs unqualified)`

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (using menhir 3.0)
  > EOF
  $ cat >dune <<EOF
  > (include_subdirs unqualified)
  > (executable
  >  (name foo))
  > EOF

$ cat >ast.ml <<EOF
> type expr =
>   | Unit
> EOF
  $ mkdir -p lang/sub
  $ cat >foo.ml <<EOF
  > let () =
  >   assert
  >     (Parser.expr
  >       (fun _ -> Parser.EOF)
  >       (Lexing.from_string "")
  >       =
  >       Ast.Unit)
  > EOF
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

