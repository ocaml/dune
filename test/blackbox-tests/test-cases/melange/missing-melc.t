Test cases when melc is not available

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ cat > main_melange.ml <<EOF
  > let () =
  >   print_endline "hello from melange"
  > EOF

Set up some fake environment without melc

  $ mkdir _path
  $ ln -s $(command -v dune) _path/
  $ ln -s $(command -v ocamlc) _path/
  $ ln -s $(command -v ocamldep) _path/

For melange.emit stanzas, an error is shown

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (entries main_melange)
  >  (module_system commonjs))
  > EOF

  $ (unset INSIDE_DUNE; PATH=_path dune build --always-show-command-line --root . output/main_melange.js)
  File "dune", line 1, characters 0-81:
  1 | (melange.emit
  2 |  (target output)
  3 |  (entries main_melange)
  4 |  (module_system commonjs))
  Error: Program melc not found in the tree or in PATH
   (context: default)
  Hint: opam install melange
  [1]

For libraries, if no melange.emit stanza is found, build does not fail

  $ cat > dune <<EOF
  > (library
  >  (name lib1)
  >  (modules :standard \ main_native)
  >  (modes byte melange))
  > (executable
  >  (name main_native)
  >  (modules main_native)
  >  (modes exe byte)
  >  (libraries lib1))
  > EOF

  $ cat > main_native.ml <<EOF
  > let () =
  >   print_endline Lib1.Lib.t
  > EOF

  $ cat > lib.ml <<EOF
  > let t = "hello from native"
  > EOF

  $ (unset INSIDE_DUNE; PATH=_path dune build --always-show-command-line --root . main_native.bc)
  $ dune exec ./main_native.bc
  hello from native

If melange.emit stanza is found, but no rules are executed, build does not fail

  $ cat > dune <<EOF
  > (library
  >  (name lib1)
  >  (modules :standard \ main_native main_melange)
  >  (modes byte melange))
  > (executable
  >  (name main_native)
  >  (modules main_native)
  >  (modes exe byte)
  >  (libraries lib1))
  > (melange.emit
  >  (target output)
  >  (entries main_melange)
  >  (libraries lib1)
  >  (module_system commonjs))
  > EOF

  $ (unset INSIDE_DUNE; PATH=_path dune build --always-show-command-line --root . main_native.bc)
  $ dune exec ./main_native.bc
  hello from native

But trying to build any melange artifacts will fail

  $ (unset INSIDE_DUNE; PATH=_path dune build --always-show-command-line --root . output/main_melange.js)
  File ".lib1.objs/melange/_unknown_", line 1, characters 0-0:
  Error: Program melc not found in the tree or in PATH
   (context: default)
  Hint: opam install melange
  [1]
