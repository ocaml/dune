Merlin must supply private CMIs from hidden transitive Melange dependencies.

  $ make_melange_project 3.25 1.0
  $ echo '(implicit_transitive_deps false)' >> dune-project
  $ mkdir lib bridge consumer
  $ cat > lib/dune <<'EOF'
  > (library
  >  (name dep)
  >  (modes melange)
  >  (private_modules private))
  > EOF
  $ echo 'let answer = 42' > lib/private.ml
  $ echo 'module P = Private' > lib/api.ml
  $ echo '(library (name bridge) (modes melange) (libraries dep))' > bridge/dune
  $ echo 'module P = Dep.Api.P' > bridge/bridge.ml
  $ cat > consumer/dune <<'EOF'
  > (library
  >  (name consumer)
  >  (modes melange)
  >  (libraries bridge))
  > EOF
  $ echo 'let answer = Bridge.P.answer' > consumer/consumer.ml

Build the dependency interfaces and the consumer's Merlin configuration.

  $ dune build \
  >   lib/.dep.objs/melange/dep__Private.cmi \
  >   bridge/.bridge.objs/melange/bridge.cmi \
  >   consumer/.merlin-conf/lib-consumer
  $ dune ocaml merlin dump-config --format=json "$PWD/consumer" \
  > | jq '[.[] | select(.module_name == "Consumer") | .config[]] | unique' \
  > > merlin.json

Only the public CMI directory of the transitive dependency is hidden.

  $ jq_dune -r '.[]
  > | select(.[0] == "B" or .[0] == "BH" or .[0] == "CMT")
  > | select(.[1] | contains("/lib/.dep.objs/"))
  > | "\(.[0]) \(.[1])"' merlin.json
  BH $TESTCASE_ROOT/_build/default/lib/.dep.objs/public_cmi_melange
  CMT $TESTCASE_ROOT/_build/default/lib/.dep.objs/melange

Replay the generated include paths. The alias cannot be resolved.

  $ jq -r '.[] | select(.[0] == "B" or .[0] == "BH")
  > | [(if .[0] == "B" then "-I" else "-H" end), .[1]]
  > | @sh' merlin.json > includes
  $ ! xargs melc -color never --mel-stop-after-cmj -o replay.cmj \
  >   consumer/consumer.ml < includes
  File "consumer/consumer.ml", line 1, characters 13-21:
  1 | let answer = Bridge.P.answer
                   ^^^^^^^^
  Error: The module Bridge.P is an alias for module Dep__Private, which is missing

Adding the missing hidden path restores typing. Neither the dependency nor
its private compilation unit becomes directly available.

  $ xargs melc -color never --mel-stop-after-cmj -o replay.cmj \
  >   -H "$PWD/_build/default/lib/.dep.objs/melange" \
  >   consumer/consumer.ml < includes
  $ echo 'let answer = Dep.Api.P.answer' > consumer/consumer.ml
  $ ! xargs melc -color never --mel-stop-after-cmj -o replay.cmj \
  >   -H "$PWD/_build/default/lib/.dep.objs/melange" \
  >   consumer/consumer.ml < includes
  File "consumer/consumer.ml", line 1, characters 13-16:
  1 | let answer = Dep.Api.P.answer
                   ^^^
  Error: Unbound module Dep
  $ echo 'let answer = Dep__Private.answer' > consumer/consumer.ml
  $ ! xargs melc -color never --mel-stop-after-cmj -o replay.cmj \
  >   -H "$PWD/_build/default/lib/.dep.objs/melange" \
  >   consumer/consumer.ml < includes
  File "consumer/consumer.ml", line 1, characters 13-25:
  1 | let answer = Dep__Private.answer
                   ^^^^^^^^^^^^
  Error: Unbound module Dep__Private
