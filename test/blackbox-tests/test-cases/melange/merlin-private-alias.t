Merlin includes hidden CMIs needed for aliases of private Melange modules.

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

The dependency's private CMI directory is a hidden include.

  $ dune build consumer/.merlin-conf/lib-consumer
  $ dune ocaml merlin dump-config --format=json "$PWD/consumer" \
  > | jq_dune -r '
  >   [merlinEntry("Consumer") | merlinConfigItemsNamed(["B", "BH"])
  >    | select(.[1] | contains("/lib/.dep.objs/"))]
  >   | unique[] | join(" ")'
  B $TESTCASE_ROOT/_build/default/lib/.dep.objs/public_cmi_melange
  BH $TESTCASE_ROOT/_build/default/lib/.dep.objs/melange
