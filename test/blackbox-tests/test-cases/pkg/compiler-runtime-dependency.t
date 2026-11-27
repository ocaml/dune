A rule that runs a tool of a locked compiler depends on the compiler package,
not only on the tool. With a relocatable OCaml 5.5, [ocaml] and [ocamlmklib]
are launchers that need the runtime [ocamlrun-a104] next to them, so a
sandboxed rule must find the whole installation in its sandbox.

The wrapper below stands in for such a compiler. Its launchers look for the
runtime next to themselves only, then delegate to the real tool:

  $ real_ocaml_bin=$(dirname "$(command -v ocamlc)")
  $ real_ocaml_lib=$(ocamlc -where)

  $ mkdir fake-compiler
  $ {
  >   echo '#!/bin/sh'
  >   echo "real_ocaml_bin='$real_ocaml_bin'"
  >   cat <<'EOF'
  > tool=$(basename "$0")
  > case "$tool" in
  > ocaml | ocamlmklib)
  >   if ! [ -x "$(dirname "$0")/ocamlrun-a104" ]; then
  >     echo "ocamlrun-a104 not found alongside $tool" >&2
  >     exit 2
  >   fi ;;
  > ocamlrun-a104) exit 0 ;;
  > esac
  > exec "$real_ocaml_bin/$tool" "$@"
  > EOF
  > } > fake-compiler/compiler
  $ chmod +x fake-compiler/compiler

As in a real lock directory, the package named by [(ocaml ..)] is a meta
package and one of its dependencies installs the binaries:

  $ make_lockdir
  $ cat >> dune.lock/lock.dune <<'EOF'
  > (ocaml ocaml-base-compiler)
  > EOF
  $ make_lockpkg relocatable-compiler <<EOF
  > (version 5.2.1)
  > EOF
  $ make_lockpkg ocaml-compiler <<EOF
  > (version 5.2.1)
  > (depends relocatable-compiler)
  > (install
  >  (progn
  >   (run mkdir -p %{prefix}/bin %{prefix}/lib/ocaml)
  >   (run sh -c
  >    "for t in ocamlc ocamlc.opt ocamldep ocamldep.opt ocamlmklib \
  >       ocamlobjinfo ocamlopt ocamlopt.opt ocaml ocamlrun-a104; \
  >     do cp compiler %{prefix}/bin/\$t; done")
  >   (run cp $real_ocaml_lib/Makefile.config
  >        %{prefix}/lib/ocaml/Makefile.config)))
  > (source (copy $PWD/fake-compiler))
  > EOF
  $ make_lockpkg ocaml-base-compiler <<EOF
  > (version 5.2.1)
  > (depends ocaml-compiler)
  > EOF

  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.24)
  > (pkg enabled)
  > (context default)
  > EOF
  $ cat > dune-project <<'EOF'
  > (lang dune 3.24)
  > (package
  >  (name repro)
  >  (allow_empty)
  >  (depends ocaml-base-compiler))
  > EOF

  $ cat > script.ml <<'EOF'
  > print_endline "script ran"
  > EOF
  $ cat > stub.c <<'EOF'
  > int repro_stub(void) { return 0; }
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (with-stdout-to out (run %{ocaml} %{dep:script.ml})))
  > (foreign_library
  >  (archive_name repro)
  >  (language c)
  >  (names stub))
  > EOF

A user rule running the toplevel:

  $ dune build ./out
  $ cat _build/default/out
  script ran

The built-in rule for the dynamic archive of a foreign library, which is always
sandboxed:

  $ dune build ./dllrepro.so
