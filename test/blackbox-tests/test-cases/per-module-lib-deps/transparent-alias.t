Baseline: transparent module aliases and library dependency recompilation.

When a module re-exports a library via a transparent alias (module M = Mylib),
consumers that use the alias must be recompiled when the library changes.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library (name mylib))
  > EOF
  $ cat > lib/mylib.ml <<EOF
  > let v = 42
  > EOF
  $ cat > lib/mylib.mli <<EOF
  > val v : int
  > EOF

  $ cat > dune <<EOF
  > (executable (name main) (libraries mylib))
  > EOF
  $ cat > re.ml <<EOF
  > module M = Mylib
  > EOF
  $ cat > re.mli <<EOF
  > module M = Mylib
  > EOF
  $ cat > main.ml <<EOF
  > let () = print_int Re.M.v
  > EOF

  $ dune build ./main.exe

Change mylib's interface:

  $ cat > lib/mylib.mli <<EOF
  > val v : int
  > val w : string
  > EOF
  $ cat > lib/mylib.ml <<EOF
  > let v = 42
  > let w = ""
  > EOF

The incremental build must succeed:

  $ dune build ./main.exe
