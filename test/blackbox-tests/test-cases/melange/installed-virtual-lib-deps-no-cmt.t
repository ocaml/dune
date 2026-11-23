An implementation of an installed Melange virtual library must conservatively
stage copied objects when binary annotations are unavailable.

  $ setup_installed_vlib_projects
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

  $ setup_no_melobjinfo_path
  $ cat > consumer/impl/virt.ml <<'EOF'
  > let run () = ignore Shared.answer
  > EOF

Reverse depends on Virt in the opposite direction. Conservative staging must
not turn copied artifacts into a Virt-to-Reverse module dependency cycle.

  $ cat > consumer/impl/reverse.ml <<'EOF'
  > let answer = Virt.run (); 0
  > EOF
  $ write_vlib_emit_stanza

  $ PATH="$no_melobjinfo_path" OCAMLPATH="$PWD/prefix/lib:$OCAMLPATH" \
  > dune build --root consumer --sandbox=symlink @melange

  $ show_installed_vlib_deps "$no_melobjinfo_path" Helper Reverse Unused Virt
  _build/default/impl/.impl.objs/melange/vlib__Helper.cmi
  _build/default/impl/.impl.objs/melange/vlib__Helper.cmj
  _build/default/impl/.impl.objs/melange/vlib__Reverse.cmi
  _build/default/impl/.impl.objs/melange/vlib__Unused.cmi
  _build/default/impl/.impl.objs/melange/vlib__Unused.cmj
  _build/default/impl/.impl.objs/melange/vlib__Virt.cmi

melobjinfo makes precise dependencies available without CMTs, so unrelated
copied objects no longer need to be staged.

  $ mkdir fake-bin
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

  $ show_installed_vlib_deps "$PWD/fake-bin:$no_melobjinfo_path" \
  >   Helper Reverse Unused
  _build/default/impl/.impl.objs/melange/vlib__Helper.cmi
  _build/default/impl/.impl.objs/melange/vlib__Helper.cmj
