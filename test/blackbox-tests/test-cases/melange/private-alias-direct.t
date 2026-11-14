A public alias needs the CMI of its private Melange target.

  $ make_melange_project 3.25 1.0
  $ mkdir lib consumer
  $ cat > lib/dune <<'EOF'
  > (library
  >  (name dep)
  >  (modes melange)
  >  (private_modules private))
  > EOF
  $ echo 'let answer = 42' > lib/private.ml
  $ echo 'module P = Private' > lib/api.ml
  $ cat > consumer/dune <<'EOF'
  > (library
  >  (name consumer)
  >  (modes melange)
  >  (libraries dep))
  > EOF
  $ echo 'let answer = Dep.Api.P.answer' > consumer/consumer.ml

The public alias resolves its private target.

  $ dune build --sandbox copy --cache=disabled \
  >   consumer/.consumer.objs/melange/consumer.cmj

The internal compilation-unit name remains unavailable.

  $ echo 'let answer = Dep__Private.answer' > consumer/consumer.ml
  $ dune build --sandbox copy --cache=disabled \
  >   consumer/.consumer.objs/melange/consumer.cmj
  File "consumer/consumer.ml", line 1, characters 13-25:
  1 | let answer = Dep__Private.answer
                   ^^^^^^^^^^^^
  Error: Unbound module Dep__Private
  [1]
