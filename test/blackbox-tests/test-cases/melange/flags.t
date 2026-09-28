Test flags and compile_flags fields on melange.emit stanza

  $ make_melange_project 3.8 0.1

Using flags field in melange.emit stanzas is not supported

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (modules main)
  >  (flags -w -14-26))
  > EOF

  $ dune build @mel
  File "dune", line 5, characters 2-7:
  5 |  (flags -w -14-26))
        ^^^^^
  Error: Unknown field "flags"
  [1]

Adds a module that contains unused var (warning 26) and illegal backlash (warning 14)

  $ cat > main.ml <<EOF
  > let t = "\e\n" in
  > print_endline "hello"
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (modules main)
  >  (alias mel))
  > EOF

Trying to build triggers both warnings

  $ dune build @mel
  File "main.ml", line 1, characters 9-11:
  1 | let t = "\e\n" in
               ^^
  Error (warning 14 [illegal-backslash]): illegal backslash escape in string.
    Hint: Single backslashes \ are reserved for escape sequences (\n, \r, ...).
    Did you check the list of OCaml escape sequences?
    To get a backslash character, escape it with a second backslash: \\.
  File "main.ml", line 1, characters 4-5:
  1 | let t = "\e\n" in
          ^
  Error (warning 26 [unused-var]): unused variable t.
  [1]

Let's ignore them using compile_flags

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (modules main)
  >  (emit_stdlib false)
  >  (alias mel)
  >  (compile_flags -w -14-26))
  > EOF

  $ dune build @mel
  $ node _build/default/output/main.js
  hello

Can also pass flags from the env stanza. Let's go back to failing state:

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (modules main)
  >  (alias mel))
  > EOF

  $ dune build @mel
  File "main.ml", line 1, characters 9-11:
  1 | let t = "\e\n" in
               ^^
  Error (warning 14 [illegal-backslash]): illegal backslash escape in string.
    Hint: Single backslashes \ are reserved for escape sequences (\n, \r, ...).
    Did you check the list of OCaml escape sequences?
    To get a backslash character, escape it with a second backslash: \\.
  File "main.ml", line 1, characters 4-5:
  1 | let t = "\e\n" in
          ^
  Error (warning 26 [unused-var]): unused variable t.
  [1]

Adding env stanza with both warnings silenced allows the build to pass successfully

  $ cat > dune <<EOF
  > (env
  >  (_
  >   (melange.compile_flags -w -14-26)))
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (emit_stdlib false)
  >  (modules main))
  > EOF

  $ dune build @mel
  $ node _build/default/output/main.js
  hello

Warning 102 (Melange only) is available if explicitly set

  $ cat > main.ml <<EOF
  > let compare a b = compare a b
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (modules main)
  >  (emit_stdlib false)
  >  (compile_flags -w +a-70))
  > EOF

  $ dune build output/main.js
  File "main.ml", line 1, characters 18-29:
  1 | let compare a b = compare a b
                        ^^^^^^^^^^^
  Warning 102 [polymorphic-comparison-introduced]: Polymorphic comparison introduced (maybe unsafe)

But it is disabled by default

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (modules main))
  > EOF

  $ dune build output/main.js

The library and env fields keep the old spelling without warnings before 3.25.
Common flags apply to both compilers, while Melange flags inherit from env and
support includes.

  $ mkdir lib
  $ cd lib
  $ make_melange_project 3.24 1.0
  $ echo 'let value = 42' > foo.ml
  $ echo '(-w +42)' > flags.sexp
  $ cat > dune <<'EOF'
  > (env
  >  (_ (melange.compile_flags -w +41)))
  > (library
  >  (name foo)
  >  (modes byte melange)
  >  (flags -w +43)
  >  (melange.compile_flags :standard (:include flags.sexp)))
  > EOF

  $ dune build foo.cma .foo.objs/melange/foo.cmj
  $ dune trace cat | jq_dune -sc '
  > [.[] | processes
  >  | select(.args.target_files // []
  >           | any(endswith(".cmo") or endswith(".cmj")))
  >  | {target: (.args.target_files
  >              | map(select(endswith(".cmo") or endswith(".cmj")))
  >              | first | basename),
  >     flags: (.args.process_args | map(select(startswith("+"))))}]
  > | sort_by(.target)'
  [{"target":"foo.cmj","flags":["+43","+41","+42"]},{"target":"foo.cmo","flags":["+43"]}]

The old spelling should be deprecated from 3.25, but currently emits no warning.

  $ make_melange_project 3.25 1.0
  $ dune build foo.cma .foo.objs/melange/foo.cmj

The new spelling should require Dune 3.25, but is currently unknown in both
language versions.

  $ sed 's/melange.compile_flags/melange.flags/g' dune > dune.new
  $ mv dune.new dune
  $ make_melange_project 3.24 1.0
  $ dune build foo.cma .foo.objs/melange/foo.cmj
  File "dune", line 2, characters 5-18:
  2 |  (_ (melange.flags -w +41)))
           ^^^^^^^^^^^^^
  Error: Unknown field "melange.flags"
  [1]

  $ make_melange_project 3.25 1.0
  $ echo 'let another = 43' >> foo.ml
  $ dune build foo.cma .foo.objs/melange/foo.cmj
  File "dune", line 2, characters 5-18:
  2 |  (_ (melange.flags -w +41)))
           ^^^^^^^^^^^^^
  Error: Unknown field "melange.flags"
  [1]
  $ dune trace cat | jq_dune -sc '
  > [.[] | processes
  >  | select(.args.target_files // []
  >           | any(endswith(".cmo") or endswith(".cmj")))
  >  | {target: (.args.target_files
  >              | map(select(endswith(".cmo") or endswith(".cmj")))
  >              | first | basename),
  >     flags: (.args.process_args | map(select(startswith("+"))))}]
  > | sort_by(.target)'
  []

Both spellings in the same library or env configuration should be rejected.

  $ cat > dune <<'EOF'
  > (library
  >  (name foo)
  >  (modes melange)
  >  (melange.flags)
  >  (melange.compile_flags))
  > EOF
  $ dune build @check
  File "dune", line 4, characters 2-15:
  4 |  (melange.flags)
        ^^^^^^^^^^^^^
  Error: Unknown field "melange.flags"
  [1]

  $ cat > dune <<'EOF'
  > (env
  >  (_
  >   (melange.flags)
  >   (melange.compile_flags)))
  > EOF
  $ dune build @check
  File "dune", line 3, characters 3-16:
  3 |   (melange.flags)
         ^^^^^^^^^^^^^
  Error: Unknown field "melange.flags"
  [1]

The new field must still require the Melange extension.

  $ make_dune_project 3.25
  $ cat > dune <<'EOF'
  > (library
  >  (name foo)
  >  (modes byte)
  >  (melange.flags))
  > EOF
  $ dune build @check
  File "dune", line 4, characters 2-15:
  4 |  (melange.flags))
        ^^^^^^^^^^^^^
  Error: Unknown field "melange.flags"
  [1]
