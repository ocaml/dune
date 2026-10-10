A library with inline tests depends on a generated source file
(`bar.ml`) whose rule depends on the library's inline-test
runtest alias. Building the library requires `bar.ml`; `bar.ml`
requires the runtest alias; the runtest alias requires the
library — a dependency cycle.

  $ make_dune_project 3.18

  $ cat >test.ml <<EOF
  > (*TEST: assert (1 = 2) *)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name backend_simple)
  >  (modules ())
  >  (inline_tests.backend
  >   (generate_runner (run sed "s/(\\\\*TEST:\\\\(.*\\\\)\\\\*)/let () = if \\"%{inline_tests}\\" = \\"enabled\\" then \\\\1;;/" %{impl-files}))))
  > 
  > (library
  >  (name foo_simple)
  >  (inline_tests (backend backend_simple)))
  > 
  > (rule
  >  (deps
  >   (alias runtest-foo_simple))
  >  (action
  >   (with-outputs-to bar.ml
  >    (echo "let message = \"Hello world\""))))
  > EOF

Dune detects this cycle and emits a "Dependency cycle between:"
error. The walk's node sequence depends on rule-scheduling order
and varies across environments, so the test filters walk nodes
via `dune_cmd delete` and asserts only the invariants: the
error header, the runtest alias node, and exit status `[1]`.

What this test catches and what it intentionally doesn't:

| Regression                                       | Caught? |
|--------------------------------------------------|---------|
| No cycle at all                                  | yes     |
| Cycle no longer involves runtest-foo_simple      | yes     |
| Cycle now involves additional aliases            | yes     |
| Cycle error header format changes                | yes     |
| Build exits 0 instead of 1                       | yes     |
| Cycle has different intermediate file/path nodes | no      |

The intermediate-path dimension is rule-scheduling-dependent;
asserting it would require periodic re-promotes that contribute
no meaningful regression coverage.
  $ dune build 2>&1 | dune_cmd delete '^(File |\d+ \||   _build|-> _build|-> required by|-> %\{|.*sed: )'
  Error: Dependency cycle between:
  -> alias runtest-foo_simple in dune:9
  [1]
