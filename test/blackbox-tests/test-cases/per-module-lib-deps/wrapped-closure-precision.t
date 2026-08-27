Observational baseline: a precision gap that any per-module dep
filter must navigate when a wrapped sibling library's wrapper
appears in a consumer's reference set, and the consumer reaches
one entry of the wrapper's transitive [Lib.closure] via an alias
chain inside one of the wrapper's children.

[dep_lib] is unwrapped with two entry modules: [reached_module]
(which the consumer transitively reaches via an alias chain) and
[unreached_module] (which no source file in the test reaches).
[lib_re_export] is wrapped (dune auto-generates the wrapper)
with a single child [pprint] that aliases only [reached_module].
[consumer_lib] depends on [lib_re_export] and writes
[Lib_re_export.Pprint.Re.x] — naming [Lib_re_export] in source.

The consumer reaches [reached_module] through [pprint]'s alias
chain. It does NOT reach [unreached_module]. A filter that walks
ocamldep through the auto-generated wrapper's children's [.d]
files could observe this and emit a per-module dep on
[reached_module.cmi] alone — editing [unreached_module.mli]
would then leave the consumer untouched.

On trunk, the consumer's compile rule globs over [dep_lib]'s
objdir (the cctx-wide [-I]/[-H] include flags), so editing
[unreached_module.mli] invalidates the consumer through that
glob. The same outcome holds for any per-module filter that
conservatively globs the entire [Lib.closure] of a wrapped
sibling in the reference set: the closure includes [dep_lib], so
the glob still fires on [unreached_module] changes. Only a
deeper filter that descends through wrapped libs' children to
find the alias chain can drop the dep on [unreached_module].

The conservative approach is sound (it can never miss a real
dep) but loses precision relative to the deeper walk. This test
records the current behaviour: editing an unreached module of a
transitively-globbed lib still rebuilds the consumer.

A future filter improvement that walks wrapped libs' children
flips the expected target_files array to [].

  $ make_dune_project 3.23

  $ cat > dune <<EOF
  > (library
  >  (name dep_lib)
  >  (wrapped false)
  >  (modules reached_module unreached_module))
  > (library
  >  (name lib_re_export)
  >  (modules pprint)
  >  (libraries dep_lib))
  > (library
  >  (name consumer_lib)
  >  (wrapped false)
  >  (modules consumer)
  >  (libraries lib_re_export))
  > EOF

  $ cat > reached_module.ml <<EOF
  > let x = "reached"
  > EOF
  $ cat > reached_module.mli <<EOF
  > val x : string
  > EOF
  $ cat > unreached_module.ml <<EOF
  > let x = "unreached"
  > EOF
  $ cat > unreached_module.mli <<EOF
  > val x : string
  > EOF

  $ cat > pprint.ml <<EOF
  > module Re = Reached_module
  > EOF

  $ cat > consumer.ml <<EOF
  > let _ = Lib_re_export.Pprint.Re.x
  > EOF

  $ dune build @check

Edit [unreached_module]'s interface — a module the consumer does
NOT reach through any alias chain ([Pprint] aliases
[Reached_module] only). Under the conservative [Lib.closure]
glob, [dep_lib]'s objdir is on the consumer's compile rule, so
the [.cmi] content change invalidates the consumer:

  $ cat > unreached_module.mli <<EOF
  > val x : string
  > val y : int
  > EOF
  $ cat > unreached_module.ml <<EOF
  > let x = "unreached"
  > let y = 1
  > EOF
  $ dune build @check
  $ dune trace cat | jq_dune -s '[.[] | targetsMatchingFilter(test("\\.consumer_lib\\.objs/byte/consumer\\."))]'
  [
    {
      "target_files": [
        "_build/default/.consumer_lib.objs/byte/consumer.cmi",
        "_build/default/.consumer_lib.objs/byte/consumer.cmo",
        "_build/default/.consumer_lib.objs/byte/consumer.cmt"
      ]
    }
  ]

The wrapped library can also be discovered after the initial consumer
references. Its closure must still take the conservative glob path.

[bridge] is unwrapped and re-exports an alias from the wrapped [middle]
library. [middle]'s child in turn aliases [leaf]:

  $ mkdir transitive-wrapped && cd transitive-wrapped
  $ make_dune_project 3.23
  $ mkdir leaf middle bridge consumer
  $ cat > leaf/dune <<EOF
  > (library (name leaf) (wrapped false))
  > EOF
  $ cat > leaf/leaf.ml <<EOF
  > let value = 42
  > EOF
  $ cat > middle/dune <<EOF
  > (library (name middle) (libraries leaf))
  > EOF
  $ cat > middle/middle_child.ml <<EOF
  > module L = Leaf
  > EOF
  $ cat > bridge/dune <<EOF
  > (library
  >  (name bridge)
  >  (wrapped false)
  >  (libraries middle))
  > EOF
  $ cat > bridge/bridge.ml <<EOF
  > module L = Middle.Middle_child.L
  > EOF
  $ cat > consumer/dune <<EOF
  > (executable (name consumer) (libraries bridge))
  > EOF
  $ cat > consumer/consumer.ml <<EOF
  > let () = print_int Bridge.L.value
  > EOF

The consumer initially references only [Bridge]. The cross-library walk then
reaches the wrapped [Middle], whose closure includes [Leaf].

  $ dune build --sandbox=copy consumer/consumer.exe
