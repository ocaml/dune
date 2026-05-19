A [(flags ... -open <not-a-module-name> ...)] must not crash dune
during rule generation. Names that do not parse as an OCaml
[Module_name.t] should be tolerated by dune's flag-handling paths;
the compiler is the authority on whether a given [-open] argument
is acceptable, and dune should not pre-empt that decision by
raising on syntactically invalid names.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

A trivial dependency library so the consumer's compile rule has
inter-library deps to filter — this exercises any future code path
that gates [-open] parsing on the presence of [(libraries ...)].

  $ mkdir dep
  $ cat > dep/dune <<EOF
  > (library (name dep))
  > EOF
  $ cat > dep/dep.ml <<EOF
  > let value = 0
  > EOF

A library with [-open] receiving an argument that is not a
syntactically valid OCaml identifier — here, [1bad] (a leading
digit). [foo] declares [(libraries dep)] so that the per-module
inter-library filter is active for [foo]'s compile rule. Rule
generation must succeed; the compiler may reject the flag at
compile time, but that is the compiler's concern, not dune's.

  $ mkdir foo
  $ cat > foo/dune <<EOF
  > (library (name foo) (libraries dep) (flags :standard -open 1bad))
  > EOF
  $ cat > foo/m.ml <<EOF
  > let _ = ()
  > EOF

Rule generation runs to completion (dune does not raise an internal
error and does not pre-empt the compiler's decision with its own
validation). The build then fails because the compiler rejects the
flag. The expected output below pins three properties at once: the
error originates from the compiler — its [File "command line
argument: ..."] preamble is unmistakably the compiler's CLI-parser
diagnostic format, not anything dune emits; dune did not abort
earlier with an internal error or its own "invalid module name"
diagnostic; and dune did not silently drop the [-open] argument,
which would have produced a successful build with no compiler error.

  $ dune build @check 2>&1
  File "command line argument: -open "1bad"", line 1, characters 0-4:
  Error: Invalid literal 1bad
  [1]
