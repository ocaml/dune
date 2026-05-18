Regression guard for cross-library transitive [.cmi] reads
through a preprocessed intermediate. A consumer references a
module from a preprocessed library; that preprocessed module's
interface mentions a type from a leaf library that the consumer
never names syntactically. The consumer's compile must depend on
the leaf's [.cmi] so the OCaml compiler can resolve the type
reference imported via the intermediate's [.cmi].

[build_lib_index] stores each entry's *post-pp* [Module.t]
(mirroring [Pp_spec.pped_modules_map]), so [cross_lib_tight_set]
reads ocamldep on the dep lib's [.pp.ml] artifact. The walker
therefore reaches the leaf's name through the intermediate's
interface, the classification fold places the leaf in the
consumer's tight per-module deps, and forced sandboxing succeeds
because [leaf]'s [.cmi] is in the sandbox.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

[leaf] exposes [Leaf.t]:

  $ mkdir leaf
  $ cat > leaf/dune <<EOF
  > (library (name leaf) (wrapped false))
  > EOF
  $ cat > leaf/leaf.ml <<EOF
  > type t = int
  > let zero : t = 0
  > EOF

[middle] depends on [leaf]; its single module is preprocessed via
[(preprocess (action ...))], and its interface mentions [Leaf.t]:

  $ mkdir middle
  $ cat > middle/dune <<EOF
  > (library
  >  (name middle)
  >  (wrapped false)
  >  (libraries leaf)
  >  (preprocess (action (run cat %{input-file}))))
  > EOF
  $ cat > middle/middle.mli <<EOF
  > val identity : Leaf.t -> Leaf.t
  > EOF
  $ cat > middle/middle.ml <<EOF
  > let identity x = x
  > EOF

[consumer] depends on [middle]; references [Middle.identity] but
never names [Leaf] in source:

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (executable (name consumer) (libraries middle))
  > EOF
Applying [Middle.identity] to a literal [0] forces the compiler
to unify [int] with [Leaf.t], which requires loading [Leaf.cmi].
ocamldep on this source still reports only [Middle] as referenced,
since [Leaf] is not named.

  $ cat > consumer/consumer.ml <<EOF
  > let _ = Middle.identity 0
  > EOF

Sandbox-forced build of the consumer succeeds: [leaf]'s [.cmi]
is declared as a compile-rule dep through the per-module
filter's cross-lib walk and is therefore present in the sandbox.

  $ dune build --sandbox=copy consumer/consumer.exe
