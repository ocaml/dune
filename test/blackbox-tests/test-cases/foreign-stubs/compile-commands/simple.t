Test that compile_commands.json is generated for C stubs.

  $ make_dune_project 3.23

Create a simple library with a C stub:

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (foreign_stubs (language c) (names stub)))
  > EOF

  $ cat >stub.c <<EOF
  > #include <caml/mlvalues.h>
  > value foo_stub(value unit) { return Val_int(42); }
  > EOF

  $ cat >foo.ml <<EOF
  > external stub : unit -> int = "foo_stub"
  > EOF

Build compile_commands.json directly:

  $ dune build compile_commands.json

The file is promoted to the workspace root:

  $ test -f compile_commands.json

Check that the file contains our stub:

  $ jq '.[0].file' compile_commands.json
  "stub.c"

Building @check also produces the file:

  $ dune clean
  $ dune build @check
  $ test -f compile_commands.json

Clean removes the promoted file:

  $ dune clean
  $ test -f compile_commands.json
  [1]

Compilation commands should identify the original source, not its build-tree
copy. Generated sources must instead use their build-tree paths, without
building them just to generate the database. Cover sources in subdirectories.

  $ mkdir -p editor/stubs
  $ cd editor
  $ make_dune_project 3.23
  $ cat > dune <<'EOF'
  > (include_subdirs unqualified)
  > (foreign_library
  >  (archive_name stubs)
  >  (language c)
  >  (names stub generated))
  > EOF
  $ cat > stubs/dune <<'EOF'
  > (rule (action (write-file generated.c "int generated(void) { return 0; }")))
  > EOF
  $ cat > stubs/stub.c <<'EOF'
  > int stub(void) { return 42; }
  > EOF
  $ dune build compile_commands.json
  $ test ! -f _build/default/stubs/generated.c

Both the working directory and file identity should be absolute.
TODO: The database currently identifies neither source correctly.

  $ jq 'all(.[]; (.directory | startswith(env.PWD + "/")) and
  >   (.file | startswith(env.PWD + "/")))' compile_commands.json
  false
  $ jq 'map({directory: (.directory | ltrimstr(env.PWD + "/")),
  >   file: (.file | ltrimstr(env.PWD + "/")),
  >   input: (.arguments[-1] | ltrimstr(env.PWD + "/"))}) |
  >   sort_by(.file)' compile_commands.json
  [
    {
      "directory": "_build/default",
      "file": "generated.c",
      "input": "generated.c"
    },
    {
      "directory": "_build/default",
      "file": "stub.c",
      "input": "stub.c"
    }
  ]
