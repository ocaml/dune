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
  >  (private_modules helper leaf unused)
  >  (virtual_modules virt other))
  > EOF
  $ cat > producer/vlib/virt.mli <<'EOF'
  > val run : unit -> int
  > EOF
  $ cat > producer/vlib/other.mli <<'EOF'
  > val run : unit -> int
  > EOF
  $ cat > producer/vlib/shared.ml <<'EOF'
  > let answer = Helper.answer + Other.run ()
  > EOF
  $ cat > producer/vlib/helper.ml <<'EOF'
  > let answer = Leaf.answer
  > EOF
  $ cat > producer/vlib/leaf.ml <<'EOF'
  > let answer = 42
  > EOF
  $ cat > producer/vlib/unused.ml <<'EOF'
  > let ignored = 0
  > EOF

  $ dune build --root producer @install
  $ dune install --root producer --prefix "$PWD/prefix"
  $ test -e "$PWD/prefix/lib/repro/vlib/melange/vlib__Shared.cmt"
  $ test -e "$PWD/prefix/lib/repro/vlib/melange/.private/vlib__Helper.cmt"
  $ test -e "$PWD/prefix/lib/repro/vlib/melange/.private/vlib__Leaf.cmt"

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
  $ cat > consumer/impl/other.ml <<'EOF'
  > let run () = 1
  > EOF
  $ cat > consumer/dune <<'EOF'
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (compile_flags :standard --mel-cross-module-opt)
  >  (libraries impl))
  > EOF

  $ OCAMLPATH="$PWD/prefix/lib:$OCAMLPATH" \
  > dune build --root consumer --sandbox=symlink @melange
  Entering directory 'consumer'
  File "impl/.impl.objs/melange/_unknown_", line 1, characters 0-0:
  Error: No rule found for impl/.impl.objs/native/vlib__Shared.cmx
  Leaving directory 'consumer'
  [1]

With annotations, dependency analysis should read the precise imports from the
CMT, including private modules but excluding unused ones.

  $ OCAMLPATH="$PWD/prefix/lib:$OCAMLPATH" \
  > dune rules --root consumer --recursive --format=json --deps --display=quiet \
  > impl/.impl.objs/melange/vlib__Virt.cmj > deps.json
  Entering directory 'consumer'
  Error: No rule found for impl/.impl.objs/native/vlib__Shared.cmx
  -> required by transitive deps of vlib__Shared.impl in _build/default/impl
  -> required by transitive deps of vlib__Virt.impl in _build/default/impl
  Leaving directory 'consumer'
  [1]
  $ jq_dune -r '
  >   [.[] | depsFilePaths
  >    | select(endswith("vlib__Helper.cmi")
  >             or endswith("vlib__Helper.cmj")
  >             or endswith("vlib__Leaf.cmi")
  >             or endswith("vlib__Leaf.cmj")
  >             or endswith("vlib__Other.cmi")
  >             or endswith("vlib__Other.cmj")
  >             or endswith("vlib__Unused.cmi")
  >             or endswith("vlib__Unused.cmj")
  >             or endswith("vlib__Virt.cmi")
  >             or endswith("vlib__Virt.cmj"))
  >    | select(startswith("_build/default/impl/.impl.objs/melange/"))]
  >   | unique[]
  > ' deps.json
