Observational baseline: a consumer module's compile command
currently carries [-I] flags for every library in the cctx-wide
[(libraries ...)] closure, regardless of which libraries the
module's source actually references.

[consumer_lib] declares two library dependencies, [dep_lib] and
[unrelated_lib], but its only module references just [Dep_module].
A future per-module filter could observe via ocamldep that the
module references nothing from [unrelated_lib] and drop that
library's objdir from the [-I] path. This test records today's
[-I] contents so that a future filter improvement can flip the
asserted array to a single entry.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat > dune <<EOF
  > (library (name dep_lib) (wrapped false) (modules dep_module))
  > (library (name unrelated_lib) (wrapped false) (modules unrelated_module))
  > (library
  >  (name consumer_lib)
  >  (wrapped false)
  >  (modules consumer_module)
  >  (libraries dep_lib unrelated_lib))
  > EOF

  $ cat > dep_module.ml <<EOF
  > let v = 1
  > EOF
  $ cat > unrelated_module.ml <<EOF
  > let _ = ()
  > EOF
  $ cat > consumer_module.ml <<EOF
  > let _ = Dep_module.v
  > EOF

  $ dune build @check

Inspect the [-I] flags on [consumer_module]'s [.cmo] compile rule.
Filter to objdir-shaped paths so we don't print the consumer's
own [.consumer_lib.objs/byte] entry — that's always present and
not what this test is about. Today both [dep_lib]'s objdir and
[unrelated_lib]'s objdir appear:

  $ dune rules --root . --format=json _build/default/.consumer_lib.objs/byte/consumer_module.cmo \
  > | jq 'include "dune"; .[] | [ruleActionFlagValues("-I") | select(test("\\.dep_lib\\.objs|\\.unrelated_lib\\.objs"))]'
  [
    ".dep_lib.objs/byte",
    ".unrelated_lib.objs/byte"
  ]
