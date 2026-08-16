Emission rules for an installed Melange library must depend on the CMJ and CMI
files in its Melange object directory.

  $ mkdir -p lib app prefix
  $ cat > lib/dune-project <<'EOF'
  > (lang dune 3.24)
  > (using melange 1.0)
  > (package (name repro))
  > EOF
  $ cat > lib/dune <<'EOF'
  > (library
  >  (name foo)
  >  (public_name repro.foo)
  >  (private_modules Classify)
  >  (modes melange))
  > EOF
  $ cat > lib/classify.ml <<'EOF'
  > let classify x = x
  > EOF
  $ cat > lib/errors.ml <<'EOF'
  > let show x = Classify.classify x
  > EOF

  $ dune build --root lib @install
  $ dune install --root lib --prefix $PWD/prefix --display=quiet
  $ ls prefix/lib/repro/foo/melange | grep '\.cmj'
  foo.cmj
  foo__Classify.cmj
  foo__Errors.cmj

  $ cat > app/dune-project <<'EOF'
  > (lang dune 3.24)
  > (using melange 1.0)
  > EOF
  $ cat > app/dune <<'EOF'
  > (melange.emit
  >  (target dist)
  >  (emit_stdlib false)
  >  (libraries repro.foo)
  >  (compile_flags :standard --mel-cross-module-opt))
  > EOF
  $ cat > app/main.ml <<'EOF'
  > let () = ignore (Foo.Errors.show 0)
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib:$OCAMLPATH \
  >   dune rules --root app --format=json --deps --display=quiet \
  >   dist/node_modules/repro.foo/errors.js > deps.json

Print the selectors so their directories can be compared with the installed
files above.

  $ jq_dune -r --arg lib_dir "$PWD/prefix/lib/repro/foo" '
  >   [.[] | depGlobEntries
  >    | select(.predicate == "*.cmj"
  >             or .predicate == "*.cmi"
  >             or .predicate == "*{.cmi,.cmj}")
  >    | select(.dir_kind == "External")
  >    | select(.dir == $lib_dir or .dir == ($lib_dir + "/melange"))
  >    | "\(.predicate) \(.dir_kind) \(.dir)"]
  >   | sort[]
  > ' deps.json
  *{.cmi,.cmj} External $TESTCASE_ROOT/prefix/lib/repro/foo/melange
