An implementation of an installed Melange virtual library must conservatively
stage copied objects when binary annotations are unavailable.

  $ mkdir -p producer/vlib consumer/impl fake-bin

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
  >  (private_modules helper unused)
  >  (virtual_modules virt reverse))
  > (env
  >  (_
  >   (bin_annot false)))
  > EOF
  $ cat > producer/vlib/virt.mli <<'EOF'
  > val run : unit -> unit
  > EOF
  $ cat > producer/vlib/reverse.mli <<'EOF'
  > val answer : int
  > EOF
  $ cat > producer/vlib/shared.ml <<'EOF'
  > let answer = Helper.answer
  > EOF
  $ cat > producer/vlib/helper.ml <<'EOF'
  > type t = int
  > let answer = 42
  > EOF
  $ cat > producer/vlib/helper.mli <<'EOF'
  > type t
  > val answer : t
  > EOF
  $ cat > producer/vlib/unused.ml <<'EOF'
  > let ignored = 0
  > EOF

  $ dune build --root producer @install
  $ dune install --root producer --prefix "$PWD/prefix"
  $ test ! -e "$PWD/prefix/lib/repro/vlib/melange/vlib__Shared.cmt"
  $ test -e "$PWD/prefix/lib/repro/vlib/melange/.private/vlib__Helper.cmi"
  $ test ! -e "$PWD/prefix/lib/repro/vlib/melange/.private/vlib__Helper.cmt"

Keep the fallback independent of whether the test environment provides
melobjinfo.

  $ mkdir no-melobjinfo-bin
  $ for program in dune melc ocamlc ocamlopt ocamldep ocamlobjinfo; do
  >   command -v "$program" > "no-melobjinfo-bin/$program.target"
  >   cat > "no-melobjinfo-bin/$program" <<'EOF'
  > #!/bin/sh
  > IFS= read -r target < "$0.target"
  > exec "$target" "$@"
  > EOF
  >   chmod +x "no-melobjinfo-bin/$program"
  > done
  $ no_melobjinfo_path="$PWD/no-melobjinfo-bin"

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
  > let run () = ignore Shared.answer
  > EOF

Reverse depends on Virt in the opposite direction. Conservative staging must
not turn copied artifacts into a Virt-to-Reverse module dependency cycle.

  $ cat > consumer/impl/reverse.ml <<'EOF'
  > let answer = Virt.run (); 0
  > EOF
  $ cat > consumer/dune <<'EOF'
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (compile_flags :standard --mel-cross-module-opt)
  >  (libraries impl))
  > EOF

  $ PATH="$no_melobjinfo_path" OCAMLPATH="$PWD/prefix/lib:$OCAMLPATH" \
  > dune build --root consumer --sandbox=symlink @melange

  $ PATH="$no_melobjinfo_path" OCAMLPATH="$PWD/prefix/lib:$OCAMLPATH" \
  > dune rules --root consumer --recursive --format=json --deps --display=quiet \
  > impl/.impl.objs/melange/vlib__Virt.cmj > deps.json
  $ jq_dune -r '
  >   [.[] | depsFilePaths
  >    | select(endswith("vlib__Helper.cmi")
  >             or endswith("vlib__Helper.cmj")
  >             or endswith("vlib__Reverse.cmi")
  >             or endswith("vlib__Reverse.cmj")
  >             or endswith("vlib__Unused.cmi")
  >             or endswith("vlib__Unused.cmj")
  >             or endswith("vlib__Virt.cmi")
  >             or endswith("vlib__Virt.cmj"))
  >    | select(startswith("_build/default/impl/.impl.objs/melange/"))]
  >   | unique[]
  > ' deps.json
  _build/default/impl/.impl.objs/melange/vlib__Helper.cmi
  _build/default/impl/.impl.objs/melange/vlib__Helper.cmj
  _build/default/impl/.impl.objs/melange/vlib__Reverse.cmi
  _build/default/impl/.impl.objs/melange/vlib__Unused.cmi
  _build/default/impl/.impl.objs/melange/vlib__Unused.cmj
  _build/default/impl/.impl.objs/melange/vlib__Virt.cmi

The current implementation ignores melobjinfo, so every copied object is still
staged.

  $ cat > fake-bin/melobjinfo <<'EOF'
  > #!/bin/sh
  > for unit do
  >   printf 'File %s\n' "$unit"
  >   printf 'Implementations imported:\n'
  >   case "$unit" in
  >     *vlib__Shared.cmj)
  >       printf '  --------------------------------  Vlib__Helper\n'
  >       ;;
  >   esac
  > done
  > EOF
  $ chmod +x fake-bin/melobjinfo

  $ PATH="$PWD/fake-bin:$no_melobjinfo_path" \
  > OCAMLPATH="$PWD/prefix/lib:$OCAMLPATH" \
  > dune rules --root consumer --recursive --format=json --deps --display=quiet \
  > impl/.impl.objs/melange/vlib__Virt.cmj > deps-with-melobjinfo.json
  $ jq_dune -r '
  >   [.[] | depsFilePaths
  >    | select(endswith("vlib__Helper.cmi")
  >             or endswith("vlib__Helper.cmj")
  >             or endswith("vlib__Reverse.cmi")
  >             or endswith("vlib__Reverse.cmj")
  >             or endswith("vlib__Unused.cmi")
  >             or endswith("vlib__Unused.cmj"))
  >    | select(startswith("_build/default/impl/.impl.objs/melange/"))]
  >   | unique[]
  > ' deps-with-melobjinfo.json
  _build/default/impl/.impl.objs/melange/vlib__Helper.cmi
  _build/default/impl/.impl.objs/melange/vlib__Helper.cmj
  _build/default/impl/.impl.objs/melange/vlib__Reverse.cmi
  _build/default/impl/.impl.objs/melange/vlib__Unused.cmi
  _build/default/impl/.impl.objs/melange/vlib__Unused.cmj
