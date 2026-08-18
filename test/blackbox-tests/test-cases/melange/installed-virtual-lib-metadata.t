An implementation of an installed Melange-only virtual library should use the
available Melange module metadata.

  $ mkdir -p producer/vlib consumer/impl

  $ cat > producer/dune-project <<'EOF'
  > (lang dune 3.24)
  > (using melange 1.0)
  > (package (name repro))
  > EOF
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
  > let run () = 42
  > EOF

  $ OCAMLPATH="$PWD/prefix/lib:$OCAMLPATH" \
  > dune build --display=quiet --root consumer consumer.install
