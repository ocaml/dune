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

Both CMI directories of the transitive dependency remain hidden.

  $ jq_dune -r '.[]
  > | select(.[0] == "B" or .[0] == "BH" or .[0] == "CMT")
  > | select(.[1] | contains("/lib/.dep.objs/"))
  > | "\(.[0]) \(.[1])"' merlin.json
  BH $TESTCASE_ROOT/_build/default/lib/.dep.objs/melange
  BH $TESTCASE_ROOT/_build/default/lib/.dep.objs/public_cmi_melange
  CMT $TESTCASE_ROOT/_build/default/lib/.dep.objs/melange

Replay the generated include paths. The public alias can be resolved.

  $ jq -r '.[] | select(.[0] == "B" or .[0] == "BH")
  > | [(if .[0] == "B" then "-I" else "-H" end), .[1]]
  > | @sh' merlin.json > includes
  $ xargs melc -color never --mel-stop-after-cmj -o replay.cmj \
  >   consumer/consumer.ml < includes

Neither the dependency nor its private compilation unit is directly available.

  $ echo 'let answer = Dep.Api.P.answer' > consumer/consumer.ml
  $ ! xargs melc -color never --mel-stop-after-cmj -o replay.cmj \
  >   consumer/consumer.ml < includes 2> errors
  $ sed -n '/^Error:/p' errors
  Error: Unbound module Dep
  $ echo 'let answer = Dep__Private.answer' > consumer/consumer.ml
  $ ! xargs melc -color never --mel-stop-after-cmj -o replay.cmj \
  >   consumer/consumer.ml < includes 2> errors
  $ sed -n '/^Error:/p' errors
  Error: Unbound module Dep__Private
