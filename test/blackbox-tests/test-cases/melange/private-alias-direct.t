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

The public alias currently fails to resolve its private target.

  $ dune build consumer/.consumer.objs/melange/consumer.cmj
  File "consumer/consumer.ml", line 1, characters 13-22:
  1 | let answer = Dep.Api.P.answer
                   ^^^^^^^^^
  Error: The module Dep.Api.P is an alias for module Dep__Private, which is missing
  [1]

A fresh copy sandbox has the same failure.

  $ dune build --sandbox copy --cache=disabled --build-dir _build-copy \
  >   consumer/.consumer.objs/melange/consumer.cmj
  File "consumer/consumer.ml", line 1, characters 13-22:
  1 | let answer = Dep.Api.P.answer
                   ^^^^^^^^^
  Error: The module Dep.Api.P is an alias for module Dep__Private, which is missing
  [1]

The internal compilation-unit name remains unavailable.

  $ echo 'let answer = Dep__Private.answer' > consumer/consumer.ml
  $ dune build --sandbox copy --cache=disabled --build-dir _build-copy \
  >   consumer/.consumer.objs/melange/consumer.cmj
  File "consumer/consumer.ml", line 1, characters 13-25:
  1 | let answer = Dep__Private.answer
                   ^^^^^^^^^^^^
  Error: Unbound module Dep__Private
  [1]
