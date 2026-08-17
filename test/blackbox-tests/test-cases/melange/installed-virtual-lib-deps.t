An implementation of an installed Melange virtual library must analyze the
installed modules using Melange object files.

  $ mkdir -p producer/vlib consumer/impl

  $ cat > producer/dune-project <<'EOF'
  > (lang dune 3.24)
  > (using melange 1.0)
  > (package (name repro))
  > EOF

The virtual library and its implementation are both Melange-only.

  $ cat > producer/vlib/dune <<'EOF'
  > (library
  >  (name vlib)
  >  (public_name repro.vlib)
  >  (modes melange)
  >  (virtual_modules virt))
  > EOF
  $ cat > producer/vlib/virt.mli <<'EOF'
  > val run : unit -> int
  > EOF
  $ cat > producer/vlib/shared.ml <<'EOF'
  > let answer = 42
  > EOF

  $ dune build --root producer @install
  $ dune install --root producer --prefix "$PWD/prefix"

  $ cat > consumer/dune-project <<'EOF'
  > (lang dune 3.24)
  > (using melange 1.0)
  > (package (name consumer))
  > EOF
  $ cat > consumer/impl/dune <<'EOF'
  > (library
  >  (name impl)
  >  (public_name consumer.impl)
  >  (modes melange)
  >  (implements repro.vlib))
  > EOF
  $ cat > consumer/impl/virt.ml <<'EOF'
  > let run () = Shared.answer
  > EOF
  $ cat > consumer/dune <<'EOF'
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (libraries impl))
  > EOF

  $ OCAMLPATH="$PWD/prefix/lib:$OCAMLPATH" \
  > dune build --root consumer @melange
  Entering directory 'consumer'
  File "impl/.impl.objs/melange/_unknown_", line 1, characters 0-0:
  Error: No rule found for impl/.impl.objs/native/vlib__Shared.cmx
  Leaving directory 'consumer'
  [1]
