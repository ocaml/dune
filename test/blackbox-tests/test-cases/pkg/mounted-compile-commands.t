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

The dependency's public library has foreign stubs of its own and does not use
its private test library.

  $ mkdir -p _dep/tests
  $ cat > _dep/dune-project <<'EOF'
  > (lang dune 3.26)
  > (package
  >  (name dep)
  >  (depends (missing_test_dependency :with-test)))
  > EOF
  $ cat > _dep/dune <<'EOF'
  > (library
  >  (public_name dep)
  >  (foreign_stubs (language c) (names dep_stubs)))
  > EOF
  $ cat > _dep/dep.ml <<'EOF'
  > external message : unit -> string = "dep_message"
  > let message = message ()
  > EOF
  $ cat > _dep/dep_stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > #include <caml/alloc.h>
  > value dep_message(value unit) { return caml_copy_string("dependency builds"); }
  > EOF
  $ cat > _dep/tests/dune <<'EOF'
  > (library
  >  (name test_bindings)
  >  (foreign_stubs (language c) (names test_stubs))
  >  (libraries dep missing_test_dependency))
  > EOF
  $ echo 'let value = 1' > _dep/tests/test_bindings.ml
  $ echo 'void test_stub(void) {}' > _dep/tests/test_stubs.c
  $ make_dune_source_lockpkg native dep "$PWD/_dep"

Building the application succeeds without the dependency's test requirements.

  $ dune build main.exe
  $ _build/default/main.exe
  dependency builds

@check generates the workspace database without resolving the mounted tests.
The dependency's stubs were compiled and linked, but are not listed.

  $ dune build @check
  $ jq '[.[].file]' compile_commands.json
  [
    "app_stubs.c"
  ]

Authored vendored projects are still part of the workspace database.

  $ echo '(vendored_dirs vendor)' >> dune
  $ mkdir vendor
  $ echo '(lang dune 3.26)' > vendor/dune-project
  $ cat > vendor/dune <<'EOF'
  > (library
  >  (name vendored)
  >  (foreign_stubs (language c) (names vendor_stubs)))
  > EOF
  $ echo 'let value = 1' > vendor/vendored.ml
  $ echo 'void vendor_stub(void) {}' > vendor/vendor_stubs.c
  $ dune build @compile_commands
  $ jq '[.[].file] | sort' compile_commands.json
  [
    "app_stubs.c",
    "vendor_stubs.c"
  ]

Missing dependencies in authored foreign libraries must still be reported.

  $ mkdir broken
  $ cat > broken/dune <<'EOF'
  > (library
  >  (name broken)
  >  (foreign_stubs (language c) (names broken_stubs))
  >  (libraries missing_workspace_dependency))
  > EOF
  $ echo 'let value = 1' > broken/broken.ml
  $ echo 'void broken_stub(void) {}' > broken/broken_stubs.c
  $ dune build @compile_commands
  File "broken/dune", line 4, characters 12-40:
  4 |  (libraries missing_workspace_dependency))
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Library "missing_workspace_dependency" not found.
  -> required by library "broken" in _build/default/broken
  -> required by _build/default/.dune/compile_commands.json
  -> required by alias compile_commands
  [1]
