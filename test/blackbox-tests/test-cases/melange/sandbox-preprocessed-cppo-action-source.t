Melange sources generated from cppo-style inputs are copied to `.melange_src`
before preprocessing. If an action preprocessor emits locations that still
point at that copied source, the compile sandbox must include the copied
preprocessed input.

The compiler does not render source previews for diagnostics that come from
textual line directives, so this case only tracks the generated cppo-shaped
source flow.

  $ mkdir -p cases/src

  $ cat > cases/dune-project <<'EOF'
  > (lang dune 3.18)
  > (using melange 0.1)
  > EOF

  $ cat > cases/src/dune <<'EOF'
  > (rule
  >  (target foo.ml)
  >  (deps foo.cppo.ml)
  >  (action
  >   (copy %{deps} %{target})))
  > 
  > (library
  >  (name cppo_action)
  >  (modes melange)
  >  (modules foo)
  >  (preprocess
  >   (action
  >    (run sh -c "printf '# 1 \"%s\"\\nlet x : nope = ()\\n' \"$1\"" -- %{input-file}))))
  > EOF

  $ cat > cases/src/foo.cppo.ml <<'EOF'
  > let x = 1
  > EOF

  $ dune build --root cases --sandbox=symlink @src/all
  Entering directory 'cases'
  File "src/.melange_src/foo.ml", line 1, characters 8-12:
  Error: Unbound type constructor nope
  Leaving directory 'cases'
  [1]
