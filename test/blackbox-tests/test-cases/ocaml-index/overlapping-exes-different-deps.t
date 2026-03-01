Test for https://github.com/ocaml/dune/issues/13566
@ocaml-index fails when multiple executables have different library
dependencies but share modules due to missing (modules) field.

  $ mkdir bin
  $ ln -s $(which ocaml_index) bin/ocaml-index
  $ export PATH=bin:$PATH

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

Create two local libraries with different modules:
  $ mkdir lib_a lib_b

  $ cat > lib_a/dune <<EOF
  > (library (name lib_a))
  > EOF

  $ cat > lib_a/lib_a.ml <<EOF
  > let value_a = "from lib_a"
  > EOF

  $ cat > lib_b/dune <<EOF
  > (library (name lib_b))
  > EOF

  $ cat > lib_b/lib_b.ml <<EOF
  > let value_b = "from lib_b"
  > EOF

Two executables with different library dependencies:
  $ cat > dune <<EOF
  > (executable
  >  (name foo)
  >  (libraries lib_a))
  > (executable
  >  (name bar)
  >  (libraries lib_b))
  > EOF

  $ cat > foo.ml <<EOF
  > let () = print_endline Lib_a.value_a
  > EOF

  $ cat > bar.ml <<EOF
  > let () = print_endline Lib_b.value_b
  > EOF

shared.ml uses Lib_a, which foo has but bar doesn't:
  $ cat > shared.ml <<EOF
  > let x = Lib_a.value_a
  > EOF

Normal build succeeds because each executable only links what it needs:
  $ dune build ./foo.exe ./bar.exe

ocaml-index succeeds because each executable only indexes modules reachable
from its entry point (shared.ml is not reachable from bar.ml):
  $ dune build @ocaml-index
