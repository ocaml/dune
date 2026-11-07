Merlin must supply hidden CMIs for public aliases of private Melange modules.

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

Build the dependency interfaces and the consumer's Merlin configuration.

  $ dune build \
  >   lib/.dep.objs/public_cmi_melange/dep.cmi \
  >   lib/.dep.objs/public_cmi_melange/dep__Api.cmi \
  >   lib/.dep.objs/melange/dep__Private.cmi \
  >   consumer/.merlin-conf/lib-consumer
  $ dune ocaml merlin dump-config --format=json "$PWD/consumer" \
  > | jq '[.[] | select(.module_name == "Consumer") | .config[]] | unique' \
  > > merlin.json

The private object directory supplies hidden CMIs and annotations.

  $ jq_dune -r '.[]
  > | select(.[0] == "B" or .[0] == "BH" or .[0] == "CMT")
  > | select(.[1] | contains("/lib/.dep.objs/"))
  > | "\(.[0]) \(.[1])"' merlin.json
  B $TESTCASE_ROOT/_build/default/lib/.dep.objs/public_cmi_melange
  BH $TESTCASE_ROOT/_build/default/lib/.dep.objs/melange
  CMT $TESTCASE_ROOT/_build/default/lib/.dep.objs/melange

Replay the generated include paths. The public alias can be resolved.

  $ jq -r '.[] | select(.[0] == "B" or .[0] == "BH")
  > | [(if .[0] == "B" then "-I" else "-H" end), .[1]]
  > | @sh' merlin.json > includes
  $ xargs melc -color never --mel-stop-after-cmj -o replay.cmj \
  >   consumer/consumer.ml < includes

The private compilation unit is still unavailable by its direct name.

  $ echo 'let answer = Dep__Private.answer' > consumer/consumer.ml
  $ ! xargs melc -color never --mel-stop-after-cmj -o replay.cmj \
  >   consumer/consumer.ml < includes 2> errors
  $ sed -n '/^Error:/p' errors
  Error: Unbound module Dep__Private
