Menhir inference must track dependencies on sibling modules whose names match
the library's generated alias module. Here, Foo refers to lang/foo.ml.

  $ make_menhir_project 3.21 3.0

  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name foo)
  >  (flags (:standard -w @49)))
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

An unused sibling depends on the parser. The generated alias for that sibling
must not introduce an inference dependency or require its CMI to exist.

  $ cat >lang/lang.ml <<'EOF'
  > module Foo = Foo
  > module Parser = Parser
  > module Unused = Unused
  > EOF
  $ echo 'let parse = Parser.expr' >lang/unused.ml
  $ dune build --sandbox=copy

An unguarded inner group must take precedence over the guarded outer group.
Its parser uses the same grammar but returns the inner Foo's distinct type.
The unused header alias also refers to a sibling supplied by the inner scope.

  $ mkdir lang/inner
  $ cp lang/dune lang/inner/dune
  $ echo 'type expr = Unit | Inner' >lang/inner/foo.ml
  $ touch lang/inner/header_only.ml
  $ cat >lang/inner/parser.mly <<'EOF'
  > %{
  > module Unused_alias = Header_only
  > %}
  > EOF
  $ cat lang/parser.mly >>lang/inner/parser.mly
  $ echo 'module Inner = Inner' >>lang/lang.ml
  $ cat >>consumer.ml <<'EOF'
  > let inner : Lang.Inner.Foo.expr =
  >   Lang.Inner.Parser.expr
  >     (fun _ -> Lang.Inner.Parser.EOF)
  >     (Lexing.from_string "")
  > EOF
  $ dune build --sandbox=copy

The inference helper must not collide with a logical module used by the parser.

  $ mock=_build/default/lang/parser__mock.ml.mock
  $ helper=$(sed -n '1s/open! struct include \(.*\) end/\1/p' "$mock")
  $ echo 'let value = Foo.Unit' >"lang/$helper.ml"
  $ cat >lang/parser.mly <<EOF
  > %token EOF
  > %start <Foo.expr> expr
  > %%
  > expr: EOF { $helper.value }
  > EOF
  $ dune build --sandbox=copy
  $ head -n 1 _build/default/lang/parser__mock.ml.mock
  open! struct include Dune__menhir__41beae67e0fd7ded637e837660c6a512_1 end

An unused alias for a missing module in the user's header must report warning
49, even with -no-alias-deps. Check inference alone so the final parser
compilation cannot mask this distinction.

  $ cat >>dune <<'EOF'
  > (env
  >  (_
  >   (flags (:standard -no-alias-deps))))
  > EOF
  $ cat >lang/parser.mly <<'EOF'
  > %{
  > module Unused_alias = Missing
  > %}
  > %token EOF
  > %start <Foo.expr> expr
  > %%
  > expr: EOF { Foo.Unit }
  > EOF
  $ dune build lang/parser__mock.mli.inferred
  File "lang/parser.mly", line 2, characters 22-29:
  Error (warning 49 [no-cmi-file]): no cmi file was found
    in path for module Missing
  [1]
