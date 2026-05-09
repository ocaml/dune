`melange.emit` is added to `@check` and performs cycle checks

  $ make_melange_project 3.8 0.1

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (emit_stdlib false))
  > EOF
  $ cat > hello.ml <<EOF
  > let x = Main.x
  > EOF
  $ cat > main.ml <<EOF
  > let x = Hello.x
  > EOF

  $ dune build .output.mobjs/melange/melange__Main.cmi
  Error: Dependency cycle between:
     transitive deps of melange__Main.impl in _build/default
  -> transitive deps of melange__Hello.impl in _build/default
  -> transitive deps of melange__Main.impl in _build/default
  -> required by _build/default/.output.mobjs/melange/melange__Main.cmi
  [1]

The check alias must also report the cycle, regardless of which module is
checked first.

  $ dune build @check > .#check-output 2>&1
  [1]
  $ grep -E \
  >   '^(Error: Dependency cycle between:|-> required by alias check)$' \
  >   .#check-output
  Error: Dependency cycle between:
  -> required by alias check
