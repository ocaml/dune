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

The library and env fields keep the old spelling without warnings before 3.26.
Common flags apply to both compilers, while Melange flags inherit from env and
support includes.

  $ mkdir lib
  $ cd lib
  $ make_melange_project 3.25 1.0
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

The old spelling remains accepted but is deprecated from 3.26.

  $ make_melange_project 3.26 1.0
  $ dune build foo.cma .foo.objs/melange/foo.cmj
  File "dune", line 2, characters 4-34:
  2 |  (_ (melange.compile_flags -w +41)))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: 'melange.compile_flags' was deprecated in version 3.26 of the dune
  language. Use 'melange.flags' instead.
  File "dune", line 7, characters 1-56:
  7 |  (melange.compile_flags :standard (:include flags.sexp)))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: 'melange.compile_flags' was deprecated in version 3.26 of the dune
  language. Use 'melange.flags' instead.

The new spelling requires Dune 3.26.

  $ sed 's/melange.compile_flags/melange.flags/g' dune > dune.new
  $ mv dune.new dune
  $ make_melange_project 3.25 1.0
  $ dune build foo.cma .foo.objs/melange/foo.cmj
  File "dune", line 2, characters 4-26:
  2 |  (_ (melange.flags -w +41)))
          ^^^^^^^^^^^^^^^^^^^^^^
  Error: 'melange.flags' is only available since version 3.26 of the dune
  language. Please update your dune-project file to have (lang dune 3.26).
  [1]

  $ make_melange_project 3.26 1.0
  $ echo 'let another = 43' >> foo.ml
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
  [{"target":"foo.cmj","flags":["+41","+42"]},{"target":"foo.cmo","flags":["+43"]}]

Both fields in the same library or env configuration are rejected.

  $ cat > dune <<'EOF'
  > (library
  >  (name foo)
  >  (modes melange)
  >  (melange.flags)
  >  (melange.compile_flags))
  > EOF
  $ dune build @check
  File "dune", line 5, characters 1-24:
  5 |  (melange.compile_flags))
       ^^^^^^^^^^^^^^^^^^^^^^^
  Warning: 'melange.compile_flags' was deprecated in version 3.26 of the dune
  language. Use 'melange.flags' instead.
  File "dune", lines 1-5, characters 0-80:
  1 | (library
  2 |  (name foo)
  3 |  (modes melange)
  4 |  (melange.flags)
  5 |  (melange.compile_flags))
  Error: fields "melange.flags" and "melange.compile_flags" are mutually
  exclusive.
  [1]

  $ cat > dune <<'EOF'
  > (env
  >  (_
  >   (melange.flags)
  >   (melange.compile_flags)))
  > EOF
  $ dune build @check
  File "dune", line 4, characters 2-25:
  4 |   (melange.compile_flags)))
        ^^^^^^^^^^^^^^^^^^^^^^^
  Warning: 'melange.compile_flags' was deprecated in version 3.26 of the dune
  language. Use 'melange.flags' instead.
  File "dune", lines 2-4, characters 1-48:
  2 |  (_
  3 |   (melange.flags)
  4 |   (melange.compile_flags)))
  Error: fields "melange.flags" and "melange.compile_flags" are mutually
  exclusive.
  [1]

The new field works without an explicit Melange extension declaration in 3.26.

  $ make_dune_project 3.26
  $ cat > dune <<'EOF'
  > (library
  >  (name foo)
  >  (modes byte)
  >  (melange.flags))
  > EOF
  $ dune build @check

melange.flags overrides the library's shared flags for Melange, while the
deprecated melange.compile_flags keeps adding to them. In both fields,
:standard inherits Melange flags from env.
When neither Melange field is set, the shared flags are used as a fallback.

  $ cd ..
  $ for field in melange.flags melange.compile_flags; do
  >   mkdir "$field"
  >   (
  >     cd "$field"
  >     case "$field" in
  >       melange.flags) make_melange_project 3.26 1.0 ;;
  >       melange.compile_flags) make_melange_project 3.25 1.0 ;;
  >     esac
  >     echo 'let value = 42' > foo.ml
  >     for flags in '-w +44' ':standard -w +44' '' omitted; do
  >       case "$flags" in
  >         omitted) env_melange_flags=; melange_flags= ;;
  >         *)
  >           env_melange_flags="($field -w +42)"
  >           melange_flags="($field $flags)"
  >           ;;
  >       esac
  >       cat > dune <<EOF
  > (env
  >  (_
  >   (flags -w +41)
  >   $env_melange_flags))
  > (library
  >  (name foo)
  >  (modes byte native melange)
  >  (flags -w +43)
  >  (ocamlc_flags -w +45)
  >  (ocamlopt_flags -w +46)
  >  $melange_flags)
  > EOF
  >       echo "$field: ${flags:-empty}"
  >       dune build foo.cma foo.cmxa .foo.objs/melange/foo.cmj
  >       dune trace cat | jq_dune -sc '
  >       [.[] | processes
  >        | select(.args.target_files // []
  >                 | any(endswith(".cmj") or endswith(".cmo")
  >                       or endswith(".cmx")))
  >        | {target: (.args.target_files
  >                    | map(select(endswith(".cmj") or endswith(".cmo")
  >                                 or endswith(".cmx")))
  >                    | first | basename),
  >           flags: (.args.process_args | map(select(startswith("+"))))}]
  >       | sort_by(.target)'
  >     done
  >   )
  > done
  melange.flags: -w +44
  [{"target":"foo.cmj","flags":["+44"]},{"target":"foo.cmo","flags":["+43","+45"]},{"target":"foo.cmx","flags":["+43","+46"]}]
  melange.flags: :standard -w +44
  [{"target":"foo.cmj","flags":["+42","+44"]}]
  melange.flags: empty
  [{"target":"foo.cmj","flags":[]}]
  melange.flags: omitted
  [{"target":"foo.cmj","flags":["+43"]}]
  melange.compile_flags: -w +44
  [{"target":"foo.cmj","flags":["+43","+44"]},{"target":"foo.cmo","flags":["+43","+45"]},{"target":"foo.cmx","flags":["+43","+46"]}]
  melange.compile_flags: :standard -w +44
  [{"target":"foo.cmj","flags":["+43","+42","+44"]}]
  melange.compile_flags: empty
  [{"target":"foo.cmj","flags":["+43"]}]
  melange.compile_flags: omitted
  [{"target":"foo.cmj","flags":["+43"]}]
