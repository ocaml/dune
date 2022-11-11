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

For melange.emit stanzas, an error is shown

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (module_system commonjs))
  > EOF

  $ (unset INSIDE_DUNE; PATH=_path dune runtest --always-show-command-line --root .)
  Error: A melange.emit stanza was found, but the melange compiler melc is not
  available. Either install it by running [opam install melange], or remove the
  melange.emit stanzas from the project.
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

  $ (unset INSIDE_DUNE; PATH=_path dune runtest --always-show-command-line --root .)

But if melange.emit stanza is found, build will fail

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

  $ (unset INSIDE_DUNE; PATH=_path dune runtest --always-show-command-line --root .)
  Error: A melange.emit stanza was found, but the melange compiler melc is not
  available. Either install it by running [opam install melange], or remove the
  melange.emit stanzas from the project.
  [1]
