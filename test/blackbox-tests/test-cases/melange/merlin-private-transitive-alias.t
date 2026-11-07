Merlin includes private CMIs from hidden transitive Melange dependencies.

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

Both of the transitive dependency's CMI directories remain hidden.

  $ dune build consumer/.merlin-conf/lib-consumer
  $ dune ocaml merlin dump-config --format=json "$PWD/consumer" \
  > | jq_dune -r '
  >   [merlinEntry("Consumer") | merlinConfigItemsNamed(["B", "BH"])
  >    | select(.[1] | contains("/lib/.dep.objs/"))]
  >   | unique[] | join(" ")'
  BH $TESTCASE_ROOT/_build/default/lib/.dep.objs/melange
  BH $TESTCASE_ROOT/_build/default/lib/.dep.objs/public_cmi_melange
