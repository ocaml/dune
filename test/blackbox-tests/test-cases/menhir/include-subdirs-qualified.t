
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

  $ write_menhir_unit_parser_sources

Menhir parsers in qualified subdirectories should be able to refer to sibling modules:

  $ dune build

Use `merge_into`

  $ write_menhir_merge_into_sources

  $ dune build
