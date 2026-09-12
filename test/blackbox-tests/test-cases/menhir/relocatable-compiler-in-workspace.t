A relocatable compiler that is installed under the workspace root but outside
_build (e.g., as with local opam switchs) will report a standard library that
`Path.Expert.try_localize_external` would try (incorrectly) to map to a
source-tree path. Since sandboxed actions refuse source-tree dependencies, the
stdlib must stay external in that layout. This test would fail with an internal
error if the conditional guard on the reclassification were removed, and it
protects against regressions for future changes to that bit of logic. Only a
standard library inside _build is a build artifact that the sandbox must
materialize; see relocatable-compiler-sandbox.t in the pkg tests.

Install a wrapper compiler under _opam. It delegates to the test compiler and
locates its standard library relative to its own path, as a compiler
configured with --with-relative-libdir does:

  $ real_ocaml_bin=$(dirname "$(command -v ocamlc)")
  $ real_ocaml_lib=$(ocamlc -where)

  $ mkdir -p _opam/bin _opam/lib/ocaml
  $ {
  >   cat <<EOF
  > #!/bin/sh
  > real_ocaml_bin='$real_ocaml_bin'
  > EOF
  >   cat <<'EOF'
  > tool=$(basename "$0")
  > case "$tool" in
  > ocamlc | ocamlc.opt | ocamlopt | ocamlopt.opt)
  >   self_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
  >   OCAMLLIB="$self_dir/../lib/ocaml"
  >   export OCAMLLIB
  >   ;;
  > esac
  > exec "$real_ocaml_bin/$tool" "$@"
  > EOF
  > } > compiler
  $ chmod +x compiler
  $ for t in ocamlc ocamlc.opt ocamldep ocamldep.opt ocamlmklib ocamlobjinfo \
  >   ocamlopt ocamlopt.opt ocaml; do cp compiler _opam/bin/$t; done
  $ cp $real_ocaml_lib/*.cmi $real_ocaml_lib/Makefile.config _opam/lib/ocaml/

  $ cat > dune-project <<'EOF'
  > (lang dune 3.24)
  > (using menhir 2.0)
  > EOF

  $ cat > dune <<'EOF'
  > (library
  >  (name repro))
  > (menhir
  >  (modules parser))
  > EOF

  $ cat > parser.mly <<'EOF'
  > %token <int> INT
  > %token EOF
  > %start <int> main
  > %%
  > main:
  > | i = INT EOF { i + 1 }
  > EOF

Menhir's type inference runs ocamlc -i in a sandbox. Both the default and an
explicitly requested sandbox must succeed rather than fail with an internal
error about depending on source-tree paths:

  $ PATH=$PWD/_opam/bin:$PATH dune build _build/default/parser__mock.mli.inferred
  $ PATH=$PWD/_opam/bin:$PATH dune build --sandbox symlink ./parser__mock.mli.inferred
