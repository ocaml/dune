A menhir grammar's semantic actions can reference values from libraries
declared in the parent (library)'s (libraries ...) field. Menhir's
--infer pass invokes [ocamlc -i] on a generated mock module derived
from the grammar; for the type inference to succeed, dune must put the
parent library's library deps on that mock compile's [-I] search path.
This test guards against regressions in that wiring.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > (using menhir 3.0)
  > EOF

[dep] is a small library exposing a value that the parent library's
menhir grammar will reference from a semantic action:

  $ mkdir dep
  $ cat > dep/dune <<EOF
  > (library (name dep))
  > EOF
  $ cat > dep/dep.ml <<EOF
  > let value = 42
  > EOF

[parser]: a library with [(libraries dep)] whose menhir grammar's
semantic action references [Dep.value]. The build succeeds only if
menhir's [--infer] pass sees [dep] on its [-I] path during the
[ocamlc -i] inference step.

  $ mkdir parser
  $ cat > parser/dune <<EOF
  > (library
  >  (name parser)
  >  (libraries dep))
  > (menhir (modules grammar))
  > EOF
  $ cat > parser/grammar.mly <<EOF
  > %token EOF
  > %start <int> main
  > %%
  > main: EOF { Dep.value }
  > EOF

  $ dune build @check

Confirm via the trace that menhir's [--infer] pass invoked
[ocamlc -i] on the generated mock and produced the inferred [.mli].
The presence of this event positively asserts that the [--infer]
code path ran, rather than being silently skipped:

  $ dune trace cat | jq -s 'include "dune"; [.[] | progMatching("ocamlc") | select(.process_args | index("-i") != null) | .target_files]'
  [
    [
      "_build/default/parser/grammar__mock.mli.inferred"
    ]
  ]

The same [ocamlc -i] invocation has [dep]'s objdir on its [-I]
search path. This is exactly the property the test guards: dune
must propagate the parent library's [(libraries ...)] onto the
mock compile's [-I] flags.

  $ dune trace cat | jq -s 'include "dune"; [.[] | progMatching("ocamlc") | select(.process_args | index("-i") != null) | .process_args | [.[] | select(startswith("dep/"))]]'
  [
    [
      "dep/.dep.objs/byte"
    ]
  ]
