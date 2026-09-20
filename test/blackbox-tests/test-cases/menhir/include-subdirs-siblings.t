Menhir inference must track dependencies on sibling modules whose names match
the library's generated alias module. Here, Foo refers to lang/foo.ml.

  $ make_menhir_project 3.21 3.0

  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name foo))
  > EOF

  $ cat >consumer.ml <<EOF
  > let () =
  >   assert
  >     (Lang.Parser.expr
  >       (fun _ -> Lang.Parser.EOF)
  >       (Lexing.from_string "")
  >       =
  >       Lang.Foo.Unit)
  > EOF

  $ mkdir -p lang

  $ cat >lang/foo.ml <<EOF
  > type expr =
  >   | Unit
  > EOF
  $ cat >lang/dune <<EOF
  > (menhir
  >  (modules parser))
  > EOF

  $ cat >lang/parser.mly <<EOF
  > %token EOF
  > %start <Foo.expr> expr
  > %%
  > expr:
  > | EOF { Foo.Unit }
  > EOF

  $ dune build
