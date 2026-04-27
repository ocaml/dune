`melange.emit` is added to `@check` and performs cycle checks

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

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

  $ dune build @check
  Error: Dependency cycle between:
     transitive deps of melange__Main.impl in _build/default
  -> transitive deps of melange__Hello.impl in _build/default
  -> transitive deps of melange__Main.impl in _build/default
  -> required by _build/default/.output.mobjs/melange/melange__Main.cmi
  -> required by alias check
  [1]
