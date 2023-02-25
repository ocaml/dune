Test flags and compile_flags fields on melange.emit stanza

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

Using flags field in melange.emit stanzas is not supported

  $ mkdir output
  $ cat > output/dune <<EOF
  > (melange.emit
  >  (module_system commonjs)
  >  (flags -w -14-26))
  > EOF

  $ dune build @melange
  File "output/dune", line 3, characters 2-7:
  3 |  (flags -w -14-26))
        ^^^^^
  Error: Unknown field flags
  [1]

Adds a module that contains unused var (warning 26) and illegal backlash (warning 14)

  $ cat > output/main.ml <<EOF
  > let t = "\e\n" in
  > print_endline "hello"
  > EOF

  $ cat > output/dune <<EOF
  > (melange.emit
  >  (entries main)
  >  (alias melange)
  >  (module_system commonjs))
  > EOF

Trying to build triggers both warnings

  $ dune build @melange
  File "output/main.ml", line 1, characters 9-11:
  1 | let t = "\e\n" in
               ^^
  Error (warning 14 [illegal-backslash]): illegal backslash escape in string.
  File "output/main.ml", line 1, characters 4-5:
  1 | let t = "\e\n" in
          ^
  Error (warning 26 [unused-var]): unused variable t.
  [1]

Let's ignore them using compile_flags

  $ cat > output/dune <<EOF
  > (melange.emit
  >  (entries main)
  >  (alias melange)
  >  (module_system commonjs)
  >  (compile_flags -w -14-26))
  > EOF

  $ dune build @melange
  $ node _build/default/output/output/main.js
  hello

Can also pass flags from the env stanza. Let's go back to failing state:

  $ cat > output/dune <<EOF
  > (melange.emit
  >  (entries main)
  >  (alias melange)
  >  (module_system commonjs))
  > EOF

  $ dune build @melange
  File "output/main.ml", line 1, characters 9-11:
  1 | let t = "\e\n" in
               ^^
  Error (warning 14 [illegal-backslash]): illegal backslash escape in string.
  File "output/main.ml", line 1, characters 4-5:
  1 | let t = "\e\n" in
          ^
  Error (warning 26 [unused-var]): unused variable t.
  [1]

Adding env stanza with both warnings silenced allows the build to pass successfully

  $ cat > output/dune <<EOF
  > (env
  >  (_
  >   (melange.compile_flags -w -14-26)))
  > (melange.emit
  >  (alias melange)
  >  (entries main)
  >  (module_system commonjs))
  > EOF

  $ dune build @melange
  $ node _build/default/output/output/main.js
  hello

Warning 102 (Melange only) is available if explicitly set

  $ cat > output/main.ml <<EOF
  > let compare a b = compare a b
  > EOF

  $ cat > output/dune <<EOF
  > (melange.emit
  >  (entries main)
  >  (alias melange)
  >  (compile_flags -w +a-70)
  >  (module_system commonjs))
  > EOF

  $ dune build @melange
  File "output/main.ml", line 1, characters 18-29:
  1 | let compare a b = compare a b
                        ^^^^^^^^^^^
  Warning 102 [polymorphic-comparison-introduced]: Polymorphic comparison introduced (maybe unsafe)

But it is disabled by default

  $ cat > output/dune <<EOF
  > (melange.emit
  >  (entries main)
  >  (alias melange)
  >  (module_system commonjs))
  > EOF

  $ dune build @melange
