Baseline: opaque mode interaction with library dependency recompilation.

In release profile (opaque=false), an implementation-only change triggers
recompilation of all modules in the consuming stanza due to .cmx dependencies.

In dev profile (opaque=true), local .cmx files are NOT dependencies: an
implementation-only change triggers no module recompilation at all.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name mylib))
  > EOF
  $ cat > lib/mylib.ml <<EOF
  > let value = 42
  > EOF
  $ cat > lib/mylib.mli <<EOF
  > val value : int
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mylib))
  > EOF
  $ cat > uses_lib.ml <<EOF
  > let get_value () = Mylib.value
  > EOF
  $ cat > uses_lib.mli <<EOF
  > val get_value : unit -> int
  > EOF
  $ cat > no_use_lib.ml <<EOF
  > let compute x = x + 1
  > EOF
  $ cat > no_use_lib.mli <<EOF
  > val compute : int -> int
  > EOF
  $ cat > main.ml <<EOF
  > let () =
  >   print_int (Uses_lib.get_value ());
  >   print_int (No_use_lib.compute 5)
  > EOF

--- Release profile (opaque=false): .cmx deps are tracked ---

  $ cat > dune-workspace <<EOF
  > (lang dune 3.0)
  > (profile release)
  > EOF

  $ dune build ./main.exe

Change ONLY the implementation (not the interface):

  $ cat > lib/mylib.ml <<EOF
  > let value = 43
  > EOF

No_use_lib is recompiled even though it doesn't reference Mylib:

  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("No_use_lib"))] | length'
  1

--- Dev profile (opaque=true): .cmx deps are NOT tracked for local libs ---

  $ cat > dune-workspace <<EOF
  > (lang dune 3.0)
  > (profile dev)
  > EOF

Full rebuild due to profile change:

  $ dune build ./main.exe

Change ONLY the implementation again:

  $ cat > lib/mylib.ml <<EOF
  > let value = 44
  > EOF

No modules recompile (opaque means only .cmi is a dependency, and .cmi
didn't change):

  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("dune__exe"))] | length'
  0
