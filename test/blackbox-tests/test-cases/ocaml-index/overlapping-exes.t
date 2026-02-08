Test for https://github.com/ocaml/dune/issues/13566
@ocaml-index fails when multiple executables don't specify (modules)

  $ mkdir bin
  $ ln -s $(which ocaml_index) bin/ocaml-index
  $ export PATH=bin:$PATH

Create project with two executables in same directory:
  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ cat > dune <<EOF
  > (executable (name foo))
  > (executable (name bar))
  > EOF

  $ cat > foo.ml <<EOF
  > let () = print_endline "foo"
  > EOF
  $ cat > foo.mli

  $ cat > bar.ml <<EOF
  > let () = print_endline "bar"
  > EOF

Normal build should succeed:
  $ dune build ./foo.exe ./bar.exe

Building @ocaml-index:
  $ dune build @ocaml-index
  File "bar.ml", line 1:
  Error: Could not find the .cmi file for interface bar.mli.
  [1]
