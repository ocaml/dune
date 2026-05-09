Regression test for cross-library dependency tracking through a
preprocessed library's interface.

Three libraries, all unwrapped. [other_dep] defines a record
type [Other.t]. [pp_dep] is preprocessed, depends on
[other_dep], and re-exports [Other.t] in its [.mli]. [consumer]
depends only on [pp_dep] and accesses a field of a value of
type [Other.t] without ever naming [Other].

The consumer's compile must find [other.cmi] on its include
path; otherwise the field access fails with "Unbound record
field x".

Dune normally discovers cross-library type leaks by running
ocamldep on each module's source. For preprocessed modules,
ocamldep can fail on the pre-preprocessing source, so dune
skips it — see neighbour test
[cross-lib-walk-pre-pp-source.t]. As a result, dune cannot
observe the implicit dependency from [consumer] to [other_dep]
through [pp_dep]'s interface. The consumer's include flags
must therefore conservatively retain [other_dep]'s [-I]/[-H]
even though dune sees no direct link.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

[other_dep]:

  $ mkdir other_dep
  $ cat > other_dep/dune <<EOF
  > (library (name other_dep) (wrapped false))
  > EOF
  $ cat > other_dep/other.ml <<EOF
  > type t = { x : int; y : string }
  > let make x y = { x; y }
  > EOF
  $ cat > other_dep/other.mli <<EOF
  > type t = { x : int; y : string }
  > val make : int -> string -> t
  > EOF

[pp_dep]:

  $ mkdir pp_dep
  $ cat > pp_dep/dune <<EOF
  > (library
  >  (name pp_dep)
  >  (wrapped false)
  >  (libraries other_dep)
  >  (preprocess (action (run cat %{input-file}))))
  > EOF
  $ cat > pp_dep/d.ml <<EOF
  > let make_thing () = Other.make 1 "hi"
  > EOF
  $ cat > pp_dep/d.mli <<EOF
  > val make_thing : unit -> Other.t
  > EOF

[consumer] accesses [.x] on a [D.make_thing ()] value, forcing
the type checker to load [other.cmi] even though [Other] is
never named in [c.ml]:

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (library (name consumer) (wrapped false) (libraries pp_dep))
  > EOF
  $ cat > consumer/c.ml <<EOF
  > let v = D.make_thing ()
  > let _ = v.x
  > EOF

The build must succeed:

  $ dune build @check
