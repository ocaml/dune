A mounted dependency's private foreign test libraries must not be required to
produce the workspace compilation database. Their test-only dependencies are
not part of the lock directory.

  $ make_lockdir
  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (package (name app) (depends dep))
  > EOF
  $ cat > dune <<'EOF'
  > (executable
  >  (name main)
  >  (libraries dep)
  >  (foreign_stubs (language c) (names app_stubs)))
  > EOF
  $ echo 'let () = print_endline Dep.message' > main.ml
  $ echo 'void app_stub(void) {}' > app_stubs.c

The dependency's public library does not use its private test library.

  $ mkdir -p _dep/tests
  $ cat > _dep/dune-project <<'EOF'
  > (lang dune 3.26)
  > (package
  >  (name dep)
  >  (depends (missing_test_dependency :with-test)))
  > EOF
  $ echo '(library (public_name dep))' > _dep/dune
  $ echo 'let message = "dependency builds"' > _dep/dep.ml
  $ cat > _dep/tests/dune <<'EOF'
  > (library
  >  (name test_bindings)
  >  (foreign_stubs (language c) (names test_stubs))
  >  (libraries dep missing_test_dependency))
  > EOF
  $ echo 'let value = 1' > _dep/tests/test_bindings.ml
  $ echo 'void test_stub(void) {}' > _dep/tests/test_stubs.c
  $ make_lockpkg dep <<EOF
  > (version 1.0)
  > (source (copy $PWD/_dep))
  > (build (run false))
  > EOF

Building the application succeeds without the dependency's test requirements.

  $ dune build main.exe
  $ _build/default/main.exe
  dependency builds

But @check generates compile_commands.json by resolving the foreign test
library from the mounted dependency as well.

  $ dune build @check
  File "_build/default/.lockfile/pkg/dep/tests/dune", line 4, characters 16-39:
  Error: Library "missing_test_dependency" not found.
  -> required by library "test_bindings" in
     _build/default/.lockfile/pkg/dep/tests
  -> required by _build/default/compile_commands.json
  -> required by alias check
  [1]
