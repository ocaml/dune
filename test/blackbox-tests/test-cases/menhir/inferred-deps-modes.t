Menhir dependencies must not require inference when it is disabled or a
fallback rule is unused.

  $ make_menhir_project 3.25 3.0
  $ mkdir no_infer only_tokens fallback

  $ cat > no_infer/dune <<'EOF'
  > (menhir
  >  (modules parser)
  >  (infer false))
  > (library
  >  (name no_infer))
  > EOF
  $ cat > no_infer/parser.mly <<'EOF'
  > %token EOF
  > %start <unit> main
  > %%
  > main: EOF { () }
  > EOF

An ordinary source file must not be read as internal dependency metadata.

  $ echo 'no_infer__Parser' > no_infer/parser.menhir-deps
  $ dune build no_infer/no_infer.cma

Token-only generation also skips inference, even though it is enabled by
default in the stanza.

  $ cat > only_tokens/dune <<'EOF'
  > (menhir
  >  (modules parser)
  >  (flags --only-tokens)
  >  (explain false))
  > (library
  >  (name only_tokens))
  > EOF
  $ cat > only_tokens/parser.mly <<'EOF'
  > %token EOF
  > %%
  > EOF
  $ dune build only_tokens/only_tokens.cma

Existing sources bypass the fallback rule, so its invalid grammar is not read.

  $ cat > fallback/dune <<'EOF'
  > (menhir
  >  (modules parser)
  >  (mode fallback)
  >  (explain false))
  > (library
  >  (name fallback))
  > EOF
  $ echo 'invalid grammar' > fallback/parser.mly
  $ echo 'let value = 42' > fallback/parser.ml
  $ echo 'val value : int' > fallback/parser.mli
  $ dune build fallback/fallback.cma
