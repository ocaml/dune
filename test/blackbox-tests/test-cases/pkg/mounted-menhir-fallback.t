Menhir in fallback mode looks at the sources of its directory: existing
sources bypass the rule, so the invalid grammar is not read. A project
mounted from a lock directory must behave like a workspace project
(menhir/inferred-deps-modes.t).

  $ make_lockdir
  $ make_named_package_project main 3.25 foo
  $ mkdir -p _source/fallback
  $ cat > _source/dune-project <<'EOF2'
  > (lang dune 3.25)
  > (using menhir 3.0)
  > (package (name foo) (dir .))
  > EOF2
  $ make_dune_source_lockpkg native foo "$PWD/_source"
  $ cat > _source/fallback/dune <<'EOF2'
  > (menhir
  >  (modules parser)
  >  (mode fallback)
  >  (explain false))
  > (library
  >  (name fallback))
  > EOF2
  $ echo 'invalid grammar' > _source/fallback/parser.mly
  $ echo 'let value = 42' > _source/fallback/parser.ml
  $ echo 'val value : int' > _source/fallback/parser.mli
  $ dune build _build/default/.pkg/foo/fallback/fallback.cma 2>&1

When neither generated source is present, the fallback rule must run Menhir.
The parser is generated and compiled in the mount, not by the opaque recipe.

  $ mkdir _source/generated
  $ cat > _source/generated/dune <<'EOF'
  > (menhir
  >  (modules parser)
  >  (mode fallback)
  >  (explain false))
  > (library
  >  (name generated))
  > EOF
  $ cat > _source/generated/parser.mly <<'EOF'
  > %token <int> NUMBER
  > %token EOF
  > %start <int> main
  > %%
  > main:
  > | value = NUMBER; EOF { value }
  > EOF
  $ dune build \
  >   _build/default/.pkg/foo/generated/parser.ml \
  >   _build/default/.pkg/foo/generated/parser.mli \
  >   _build/default/.pkg/foo/generated/generated.cma
