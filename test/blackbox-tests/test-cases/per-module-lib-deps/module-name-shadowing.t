Test that when a stanza's internal module name shadows a library module name,
the internal module takes precedence. This validates that ocamldep-based
dependency filtering (which treats modules defined within the same stanza as
internal) correctly reflects the compiler's resolution order.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

--- Unwrapped library: internal module shadows library module ---

An unwrapped library exposes module Helper. The executable also has a module
named Helper. The internal module takes precedence — the library's Helper is
inaccessible.

  $ mkdir unwrapped_lib
  $ cat > unwrapped_lib/dune <<EOF
  > (library
  >  (name unwrapped_lib)
  >  (wrapped false))
  > EOF

  $ cat > unwrapped_lib/helper.ml <<EOF
  > let lib_value = 42
  > EOF

  $ cat > unwrapped_lib/helper.mli <<EOF
  > val lib_value : int
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries unwrapped_lib))
  > EOF

  $ cat > helper.ml <<EOF
  > let local_value = 1
  > EOF

  $ cat > main.ml <<EOF
  > let () = print_int Helper.local_value
  > EOF

The build succeeds using the internal Helper:

  $ dune build ./main.exe

The dependencies of main.ml's native compilation show dune__exe__Helper
(the internal module), not unwrapped_lib's helper:

  $ dune rules --deps _build/default/.main.eobjs/native/dune__exe__Main.cmx | grep dune__exe__Helper
   (File (In_build_dir _build/default/.main.eobjs/byte/dune__exe__Helper.cmi))

The library's Helper.lib_value is not accessible:

  $ cat > main.ml <<EOF
  > let () = print_int Helper.lib_value
  > EOF

  $ dune build ./main.exe
  File "main.ml", line 1, characters 19-35:
  1 | let () = print_int Helper.lib_value
                         ^^^^^^^^^^^^^^^^
  Error: Unbound value Helper.lib_value
  [1]
