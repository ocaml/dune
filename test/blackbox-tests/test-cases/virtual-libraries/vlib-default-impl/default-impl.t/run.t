Basic sample selecting implementation according to default library.

  $ dune build
  hi from lib.default

Default implementations must not introduce a dependency cycle through another
virtual library. Neither implementation is selected explicitly by the program.

  $ rm lib.default/dune
  $ cat > lib.default/dune <<'EOF'
  > (library
  >  (name lib_default)
  >  (implements vlib)
  >  (libraries second))
  > EOF
  $ mkdir second second.default
  $ touch second/second.mli second.default/second.ml
  $ cat > second/dune <<'EOF'
  > (library
  >  (name second)
  >  (virtual_modules second)
  >  (default_implementation second_default))
  > EOF
  $ cat > second.default/dune <<'EOF'
  > (library
  >  (name second_default)
  >  (implements second)
  >  (libraries vlib))
  > EOF
  $ dune build bar.exe
  Error: Default implementation cycle detected between the following libraries:
     "vlib"
  -> "second_default"
  -> "second"
  -> "lib_default"
  -> "vlib"
  -> required by executable bar in dune:2
  -> required by _build/default/.bar.eobjs/native/dune__exe__Bar.cmx
  -> required by _build/default/bar.exe
  [1]
