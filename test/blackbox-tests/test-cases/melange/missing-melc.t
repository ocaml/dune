Test cases when melc is not available

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ cat > main.ml <<EOF
  > let () =
  >   print_endline "hello"
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
  >  (module_system commonjs))
  > EOF

  $ (unset INSIDE_DUNE; PATH=_path dune build --always-show-command-line --root . output/melange__Main.js)
  Error: Program melc not found in the tree or in PATH
   (context: default)
  -> required by _build/default/output/melange__Main.js
  Hint: opam install melange
  File "dune", line 1, characters 0-57:
  1 | (melange.emit
  2 |  (target output)
  3 |  (module_system commonjs))
  Error: No rule found for .output.mobjs/melange/melange__Main.cmj
  [1]

For libraries, if no melange.emit stanza is found, build does not fail

  $ cat > dune <<EOF
  > (library
  >  (name lib1)
  >  (modules :standard \ main)
  >  (modes byte melange))
  > (executable
  >  (name main)
  >  (modules main)
  >  (modes exe byte)
  >  (libraries lib1))
  > EOF

  $ cat > main.ml <<EOF
  > let () =
  >   print_endline Lib1.Lib.t
  > EOF

  $ cat > lib.ml <<EOF
  > let t = "hello"
  > EOF

  $ (unset INSIDE_DUNE; PATH=_path dune build --always-show-command-line --root . main.bc)
  $ dune exec ./main.bc
  hello

If melange.emit stanza is found, but no rules are executed, build does not fail

  $ cat > dune <<EOF
  > (library
  >  (name lib1)
  >  (modules :standard \ main main_melange)
  >  (modes byte melange))
  > (executable
  >  (name main)
  >  (modules main)
  >  (modes exe byte)
  >  (libraries lib1))
  > (melange.emit
  >  (target output)
  >  (entries main_melange)
  >  (libraries lib1)
  >  (module_system commonjs))
  > EOF

  $ cat > main_melange.ml <<EOF
  > let () =
  >   print_endline Lib1.Lib.t
  > EOF

  $ (unset INSIDE_DUNE; PATH=_path dune build --always-show-command-line --root . main.bc)
  $ dune exec ./main.bc
  hello
