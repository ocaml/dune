Per-module filtering: -open flag with no source-level references.

When a module is compiled with -open Baselib but its source doesn't
reference any module from Baselib, ocamldep reports nothing. The
-open flag extraction ensures the library is still included in deps
so the compiler can resolve the -open.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library (name baselib))
  > EOF
  $ cat > lib/baselib.ml <<EOF
  > let value = 42
  > EOF
  $ cat > lib/baselib.mli <<EOF
  > val value : int
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries baselib)
  >  (flags (:standard -open Baselib)))
  > EOF
  $ cat > no_ref.ml <<EOF
  > let compute x = x + 1
  > EOF
  $ cat > no_ref.mli <<EOF
  > val compute : int -> int
  > EOF
  $ cat > main.ml <<EOF
  > let () = print_int (No_ref.compute 5)
  > EOF

Build succeeds — no_ref.ml doesn't reference Baselib but is compiled
with -open Baselib:

  $ dune build ./main.exe
