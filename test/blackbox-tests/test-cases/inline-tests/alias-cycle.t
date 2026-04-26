In this test we check a cycle when a library depends on a genrated source file which in
turn depends on the inline-test-name alias of the inline tests of the library.

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

This kind of cycle has a difficult to understand error message. The specific
ordering of nodes in the cycle walk varies across environments and depends
on rule-scheduling order, so the test hides the walk nodes (which change)
and keeps only the invariants: the error header and the presence of the
runtest alias in the walk.
  $ dune build 2>&1 | dune_cmd delete '^(File |\d+ \||   _build|-> _build|-> required by|-> %\{|.*sed: )'
  Error: Dependency cycle between:
  -> alias runtest-foo_simple in dune:9
  [1]
