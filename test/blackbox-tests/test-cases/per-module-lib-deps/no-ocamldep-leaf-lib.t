A tight-eligible dependency library that is a leaf: one module, no
library deps of its own. Dune's [Dep_rules.skip_ocamldep] short-
circuits ocamldep for this shape — no [.d] rules exist for [leaf].
The per-module filter's cross-library walk must recognise this and
not demand a nonexistent [leaf/mod_leaf.impl.d]; otherwise the
consumer's compile fails with "No rule found" during rule
evaluation.

The guard lives in [Lib_index.no_ocamldep]. This test proves the
consumer builds successfully even though the BFS names [leaf] in
its initial frontier.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

[leaf]: unwrapped, single-module, no library dependencies. This is
exactly the shape that triggers [skip_ocamldep]:

  $ mkdir leaf
  $ cat > leaf/dune <<EOF
  > (library (name leaf) (wrapped false))
  > EOF
  $ cat > leaf/mod_leaf.ml <<EOF
  > let v = 1
  > EOF

[consumer]: multi-module library that depends on [leaf] and
references [Mod_leaf] from one of its modules. The consumer itself
has library deps, so its own ocamldep runs; what we're probing is
whether the cross-library walk tries to read [leaf]'s ocamldep:

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (library (name consumer) (wrapped false) (libraries leaf))
  > EOF
  $ cat > consumer/c.ml <<EOF
  > let _ = Mod_leaf.v
  > EOF
  $ cat > consumer/d.ml <<EOF
  > let _ = ()
  > EOF

Build must succeed. Without the [no_ocamldep] guard, the BFS would
demand [leaf/.leaf.objs/mod_leaf.impl.d] and the rule engine would
fail:

  $ dune build @check
