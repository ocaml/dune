Test melange.compile_flags, ocamlc_flags and ocamlopt_flags fields on melange.emit stanza

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

Create dune file that uses melange.compile_flags

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (entries main)
  >  (module_system commonjs)
  >  (compile_flags -w -14-26))
  > EOF

The code in main contains unused var (warning 26) and illegal backlash (warning 14)

  $ cat > main.ml <<EOF
  > let t = "\e\n" in
  > print_endline "hello"
  > EOF

Building does not fail, warnings are silenced

  $ dune build output/main.js
  $ node _build/default/output/main.js
  hello

Update dune file to use ocamlc_flags

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (entries main)
  >  (module_system commonjs)
  >  (ocamlc_flags -w -14-26))
  > EOF

Building should fail as ocamlc flags are not supported in melange emit stanzas

  $ dune build output/main.js
  File "dune", line 5, characters 2-14:
  5 |  (ocamlc_flags -w -14-26))
        ^^^^^^^^^^^^
  Error: Unknown field ocamlc_flags
  [1]

Update dune file to use ocamlopt_flags

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (entries main)
  >  (module_system commonjs)
  >  (ocamlopt_flags -w -14-26))
  > EOF

Building should fail as ocamlopt flags are not supported in melange emit stanzas

  $ dune build output/main.js
  File "dune", line 5, characters 2-16:
  5 |  (ocamlopt_flags -w -14-26))
        ^^^^^^^^^^^^^^
  Error: Unknown field ocamlopt_flags
  [1]
