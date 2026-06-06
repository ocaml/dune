In this test we check a cycle when a library depends on a genrated source file which in
turn depends on the inline-test-name alias of the inline tests of the library.

  $ make_dune_project 3.18

  $ cat >test.ml <<EOF
  > (*TEST: assert (1 = 2) *)
  > EOF

  $ write_simple_inline_tests_backend
  $ cat >>dune <<EOF
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
  Error: Dependency cycle between:
     _build/default/bar.ml
  -> transitive deps of foo_simple__Bar.impl in _build/default
  -> _build/default/.foo_simple.objs/byte/foo_simple__Bar.cmi
  -> _build/default/.foo_simple.inline-tests/.t.eobjs/native/dune__exe__Main.cmx
  -> _build/default/.foo_simple.inline-tests/inline-test-runner.exe
  -> alias runtest-foo_simple in dune:9
  -> _build/default/bar.ml
  -> required by _build/default/.foo_simple.objs/native/foo_simple.cmx
  -> required by _build/default/foo_simple.a
  -> required by alias all
  -> required by alias default
  [1]
