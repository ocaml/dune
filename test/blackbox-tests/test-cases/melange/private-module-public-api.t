Public APIs backed by private modules work in a copy sandbox.

  $ make_melange_project 3.25 1.0
  $ cat > dune <<'EOF'
  > (library
  >  (name lib)
  >  (modules private public)
  >  (modes melange)
  >  (private_modules private))
  > (melange.emit
  >  (target output)
  >  (modules main)
  >  (emit_stdlib false)
  >  (libraries lib))
  > EOF
  $ echo 'let answer = 42' > private.ml
  $ echo 'let answer = Private.answer' > public.ml
  $ echo 'let answer = Lib.Public.answer' > main.ml

  $ dune build --sandbox copy --cache=disabled output/main.js
