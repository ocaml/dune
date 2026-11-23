An implementation of an installed Melange virtual library must analyze the
installed modules using Melange object files.

  $ setup_installed_vlib_projects

The virtual library and its implementation are both Melange-only.

  $ cat > producer/vlib/dune <<'EOF'
  > (library
  >  (name vlib)
  >  (public_name repro.vlib)
  >  (modes melange)
  >  (private_modules helper leaf other_type type_only type_runtime unused)
  >  (virtual_modules virt other))
  > EOF
  $ cat > producer/vlib/virt.mli <<'EOF'
  > val run : unit -> int
  > EOF
  $ cat > producer/vlib/other.mli <<'EOF'
  > val run : Other_type.t -> int
  > EOF
  $ cat > producer/vlib/other_type.ml <<'EOF'
  > type t = int
  > EOF
  $ cat > producer/vlib/other_type.mli <<'EOF'
  > type t = int
  > EOF
  $ cat > producer/vlib/shared.ml <<'EOF'
  > type t = Type_only.t
  > let answer = Helper.answer + Other.run 0
  > EOF
  $ cat > producer/vlib/helper.ml <<'EOF'
  > let answer = Leaf.answer
  > EOF
  $ cat > producer/vlib/helper.mli <<'EOF'
  > val answer : int
  > EOF
  $ cat > producer/vlib/leaf.ml <<'EOF'
  > let answer = 42
  > EOF
  $ cat > producer/vlib/type_only.ml <<'EOF'
  > type t = int
  > let ignored = Type_runtime.value
  > EOF
  $ cat > producer/vlib/type_only.mli <<'EOF'
  > type t = int
  > val ignored : int
  > EOF
  $ cat > producer/vlib/type_runtime.ml <<'EOF'
  > let value = 0
  > EOF
  $ cat > producer/vlib/unused.ml <<'EOF'
  > let ignored = 0
  > EOF

  $ dune build --root producer @install
  $ dune install --root producer --prefix "$PWD/prefix"
  $ test -e "$PWD/prefix/lib/repro/vlib/melange/vlib__Shared.cmt"
  $ test -e "$PWD/prefix/lib/repro/vlib/melange/.private/vlib__Helper.cmt"
  $ test -e "$PWD/prefix/lib/repro/vlib/melange/.private/vlib__Leaf.cmt"

Keep the fallback independent of whether the test environment provides
melobjinfo.

  $ setup_no_melobjinfo_path
  $ cat > consumer/impl/virt.ml <<'EOF'
  > let _coerce (x : Shared.t) : int = x
  > let run () = Shared.answer
  > EOF
  $ cat > consumer/impl/other.ml <<'EOF'
  > let run _ = 1
  > EOF
  $ write_vlib_emit_stanza

  $ PATH="$no_melobjinfo_path" OCAMLPATH="$PWD/prefix/lib:$OCAMLPATH" \
  > dune build --root consumer --sandbox=symlink @melange

With annotations, dependency analysis should read the precise imports from the
CMT, including private modules but excluding unused ones.

  $ show_installed_vlib_deps "$no_melobjinfo_path" \
  >   Helper Leaf Other Other_type Type_only Type_runtime Unused Virt
  _build/default/impl/.impl.objs/melange/vlib__Helper.cmi
  _build/default/impl/.impl.objs/melange/vlib__Helper.cmj
  _build/default/impl/.impl.objs/melange/vlib__Leaf.cmi
  _build/default/impl/.impl.objs/melange/vlib__Leaf.cmj
  _build/default/impl/.impl.objs/melange/vlib__Other.cmi
  _build/default/impl/.impl.objs/melange/vlib__Other.cmj
  _build/default/impl/.impl.objs/melange/vlib__Other_type.cmi
  _build/default/impl/.impl.objs/melange/vlib__Other_type.cmj
  _build/default/impl/.impl.objs/melange/vlib__Type_only.cmi
  _build/default/impl/.impl.objs/melange/vlib__Type_only.cmj
  _build/default/impl/.impl.objs/melange/vlib__Type_runtime.cmi
  _build/default/impl/.impl.objs/melange/vlib__Type_runtime.cmj
  _build/default/impl/.impl.objs/melange/vlib__Virt.cmi

When melobjinfo is available, it takes precedence over CMT analysis.
Implementation dependencies are followed through CMJs, while interface-only
dependencies are followed through CMIs.

  $ mkdir fake-bin
  $ cat > fake-bin/melobjinfo <<'EOF'
  > #!/bin/sh
  > set -eu
  > seen_shared=false
  > seen_helper=false
  > seen_other_type=false
  > seen_type_only=false
  > for unit do
  >   printf 'File %s\n' "$unit"
  >   printf 'Implementations imported:\n'
  >   case "$unit" in
  >     *vlib__Shared.cmj)
  >       seen_shared=true
  >       printf '  --------------------------------  Vlib__Helper\n'
  >       printf '  --------------------------------  Vlib__Other\n'
  >       ;;
  >     *vlib__Helper.cmj)
  >       seen_helper=true
  >       printf '  --------------------------------  Vlib__Leaf\n'
  >       ;;
  >     *vlib__Other_type.cmj)
  >       seen_other_type=true
  >       ;;
  >     *vlib__Type_only.cmj)
  >       seen_type_only=true
  >       printf '  --------------------------------  Vlib__Type_runtime\n'
  >       ;;
  >   esac
  > done
  > test "$seen_shared" = true
  > test "$seen_helper" = true
  > test "$seen_other_type" = true
  > test "$seen_type_only" = true
  > EOF
  $ chmod +x fake-bin/melobjinfo

  $ PATH="$PWD/fake-bin:$no_melobjinfo_path" \
  > OCAMLPATH="$PWD/prefix/lib:$OCAMLPATH" \
  > dune build --root consumer --sandbox=symlink --trace-file "$PWD/trace" @melange
  $ dune trace cat --trace-file "$PWD/trace" \
  > | jq_dune -s \
  >   '[.[] | processesBrief | select(.prog == "melobjinfo")] | length'
  1

  $ show_installed_vlib_deps "$PWD/fake-bin:$no_melobjinfo_path" \
  >   Leaf Other_type Type_only Type_runtime Unused
  _build/default/impl/.impl.objs/melange/vlib__Leaf.cmi
  _build/default/impl/.impl.objs/melange/vlib__Leaf.cmj
  _build/default/impl/.impl.objs/melange/vlib__Other_type.cmi
  _build/default/impl/.impl.objs/melange/vlib__Other_type.cmj
  _build/default/impl/.impl.objs/melange/vlib__Type_only.cmi
  _build/default/impl/.impl.objs/melange/vlib__Type_only.cmj

Other is virtual, so its implementation dependency is normalized to an
interface dependency and leads to Other_type. Type_only is also part of
Shared's interface, so its CMJ-only dependency on Type_runtime is not followed.

The missing-annotation fallback is deliberately conservative at the library
level. Even when the missing CMT belongs to an unreachable module, every copied
object is staged.

  $ rm "$PWD/prefix/lib/repro/vlib/melange/.private/vlib__Unused.cmt"
  $ show_installed_vlib_deps "$no_melobjinfo_path" Unused
  _build/default/impl/.impl.objs/melange/vlib__Unused.cmi
  _build/default/impl/.impl.objs/melange/vlib__Unused.cmj
