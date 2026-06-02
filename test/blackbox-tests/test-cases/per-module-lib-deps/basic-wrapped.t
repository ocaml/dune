Baseline: library dependency recompilation for a basic wrapped library.

When a wrapped library's interface changes, Dune currently recompiles ALL
modules in stanzas that depend on the library, even modules that don't
reference it.

See: https://github.com/ocaml/dune/issues/4572

  $ make_dune_project 3.0

  $ make_value_library lib mylib 42

  $ make_mylib_consumer_executable

  $ dune build ./main.exe

Change mylib's interface:

  $ cat > lib/mylib.mli <<EOF
  > val value : int
  > val new_function : unit -> string
  > EOF
  $ cat > lib/mylib.ml <<EOF
  > let value = 42
  > let new_function () = "hello"
  > EOF

No_use_lib is recompiled even though it doesn't reference Mylib:

  $ dune build ./main.exe
  $ dune trace cat | jq_dune -s '[.[] | targetsMatchingFilter(test("No_use_lib"))] | length'
  2
