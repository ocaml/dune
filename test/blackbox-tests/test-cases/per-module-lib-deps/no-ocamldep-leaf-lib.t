A multi-module consumer of a single-module leaf library builds
without error.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

[leaf]: unwrapped, single-module, no library dependencies. This is
exactly the shape that triggers the no-ocamldep fast path:

  $ mkdir leaf
  $ cat > leaf/dune <<EOF
  > (library (name leaf) (wrapped false))
  > EOF
  $ cat > leaf/mod_leaf.ml <<EOF
  > let v = 1
  > EOF
  $ cat > leaf/mod_leaf.mli <<EOF
  > val v : int
  > EOF

[consumer]: multi-module library that depends on [leaf] and
references [Mod_leaf] from one of its modules. The consumer is
multi-module (so it doesn't itself hit the no-ocamldep fast path);
what we're probing is whether anything tries to read [leaf]'s
ocamldep:

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

Build must succeed:

  $ dune build @check
