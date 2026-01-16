In this test we check a cycle when a library depends on a genrated source file which in
turn depends on the inline-test-name alias of the inline tests of the library.

  $ cat >dune-project <<EOF
  > (lang dune 3.18)
  > EOF

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

This kind of cycle has a difficult to understand error message.
  $ dune build 2>&1 | grep -vwE "sed"
  File "dune", lines 7-9, characters 0-69:
  7 | (library
  8 |  (name foo_simple)
  9 |  (inline_tests (backend backend_simple)))
  Error: Dependency cycle between:
     _build/default/.foo_simple.objs/foo_simple__Bar.impl.all-deps
  -> _build/default/.foo_simple.objs/byte/foo_simple__Bar.cmi
  -> _build/default/.foo_simple.inline-tests/.t.eobjs/native/dune__exe__Main.cmx
  -> _build/default/.foo_simple.inline-tests/inline-test-runner.exe
  -> alias runtest-foo_simple in dune:9
  -> _build/default/bar.ml
  -> _build/default/.foo_simple.objs/foo_simple__Bar.impl.d
  -> _build/default/.foo_simple.objs/foo_simple__Bar.impl.all-deps
  -> required by _build/default/.foo_simple.objs/byte/foo_simple__Bar.cmo
  -> required by _build/default/foo_simple.cma
  -> required by alias all
  -> required by alias default
  [1]
