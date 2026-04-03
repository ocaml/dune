Per-module filtering: library dependency recompilation for unwrapped libraries.

When an unwrapped library module's interface changes, Dune currently recompiles
all modules in stanzas that depend on the library, even those referencing
different modules in the library.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

Create an unwrapped library with multiple modules:

  $ mkdir unwrapped
  $ cat > unwrapped/dune <<EOF
  > (library
  >  (name unwrapped_lib)
  >  (wrapped false))
  > EOF

  $ cat > unwrapped/helper.ml <<EOF
  > let helper_fn x = x + 10
  > EOF

  $ cat > unwrapped/helper.mli <<EOF
  > val helper_fn : int -> int
  > EOF

  $ cat > unwrapped/utils.ml <<EOF
  > let utils_fn x = x * 2
  > EOF

  $ cat > unwrapped/utils.mli <<EOF
  > val utils_fn : int -> int
  > EOF

Add modules that use different parts of the unwrapped library:

  $ cat > uses_helper.ml <<EOF
  > let call_helper () = Helper.helper_fn 5
  > EOF

  $ cat > uses_helper.mli <<EOF
  > val call_helper : unit -> int
  > EOF

  $ cat > uses_utils.ml <<EOF
  > let call_utils () = Utils.utils_fn 3
  > EOF

  $ cat > uses_utils.mli <<EOF
  > val call_utils : unit -> int
  > EOF

  $ cat > no_use_lib.ml <<EOF
  > let compute x = x + 1
  > EOF

  $ cat > no_use_lib.mli <<EOF
  > val compute : int -> int
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (modules main uses_helper uses_utils no_use_lib)
  >  (libraries unwrapped_lib))
  > EOF

  $ cat > main.ml <<EOF
  > let () =
  >   print_int (Uses_helper.call_helper ());
  >   print_int (Uses_utils.call_utils ());
  >   print_int (No_use_lib.compute 5)
  > EOF

Build first:

  $ dune build ./main.exe

Now change ONLY helper.mli (not utils.mli):

  $ cat > unwrapped/helper.mli <<EOF
  > val helper_fn : int -> int
  > val new_helper : string -> string
  > EOF

  $ cat > unwrapped/helper.ml <<EOF
  > let helper_fn x = x + 10
  > let new_helper s = s ^ "!"
  > EOF

Uses_utils is no longer recompiled because it only references Utils, not Helper:

  $ dune build ./main.exe --display short 2>&1 | grep Uses_utils
  [1]

Now change ONLY utils.mli (not helper.mli):

  $ cat > unwrapped/utils.mli <<EOF
  > val utils_fn : int -> int
  > val new_utils : string -> string
  > EOF

  $ cat > unwrapped/utils.ml <<EOF
  > let utils_fn x = x * 2
  > let new_utils s = s ^ "?"
  > EOF

Uses_helper is no longer recompiled because it only references Helper, not Utils:

  $ dune build ./main.exe --display short 2>&1 | grep Uses_helper
  [1]
