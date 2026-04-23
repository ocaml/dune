Per-module tight deps and [(wrapped (transition ...))] libraries.

A library declaring [(wrapped (transition ...))] exposes both wrapped
and bare-name entry modules for its public modules, so consumers can
refer to a module either as [Foo.Bar] or as [Bar]. Such libraries are
still wrapped ([Wrapped.to_bool] returns [true]), so per-module tight
deps do not apply: the bare-name compat shim reaches internal modules
whose cmis are not in the directly-named set. They must fall through
to the glob.

Regression guard: verify that a consumer of such a library builds
correctly.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

base is wrapped with a transition period: consumers can reach each
public module either through the wrapper (e.g. [Base.Alpha]) or by
its bare name (e.g. [Beta]):

  $ mkdir base
  $ cat > base/dune <<EOF
  > (library (name base) (wrapped (transition "transition description")))
  > EOF
  $ cat > base/alpha.ml <<EOF
  > let v = 1
  > EOF
  $ cat > base/alpha.mli <<EOF
  > val v : int
  > EOF
  $ cat > base/beta.ml <<EOF
  > let v = 2
  > EOF
  $ cat > base/beta.mli <<EOF
  > val v : int
  > EOF

Consumer intentionally exercises both access paths. The transition
machinery emits deprecation alerts for the bare-name access, so we
silence them in the consumer flags while testing both the wrapper
path and the bare-name compat shim here:

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (library
  >  (name consumer)
  >  (libraries base)
  >  (flags (:standard -alert -deprecated)))
  > EOF
  $ cat > consumer/c.ml <<EOF
  > let via_wrapper = Base.Alpha.v
  > let via_bare = Beta.v
  > EOF

  $ dune build @check
