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
  $ write_menhir_unit_parser_sources

  $ dune build

Use `merge_into`

  $ write_menhir_merge_into_sources

  $ dune build

