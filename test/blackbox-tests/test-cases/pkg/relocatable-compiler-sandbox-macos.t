A compiler installed as a regular package lives inside the build directory.
When running it in a sandbox, Dune must preserve the path to its standard
library. This matters on macOS, where the executable path retains the sandbox
symlink rather than resolving to its target.

Create a relocatable compiler package which delegates to the test compiler. The
wrapper deliberately locates its standard library relative to its own path, as
a compiler configured with --with-relative-libdir does.

  $ real_ocaml_bin=$(dirname "$(command -v ocamlc)")
  $ real_ocaml_lib=$(ocamlc -where)
  $ real_menhir=$(command -v menhir)
  $ ocaml_version=$(ocamlc -version)

  $ mkdir fake-compiler
  $ {
  >   echo "let real_ocaml_bin = \"$real_ocaml_bin\""
  >   cat <<'EOF'
  > let () =
  >   let tool = Filename.basename Sys.argv.(0) in
  >   (match tool with
  >    | "ocamlc" | "ocamlc.opt" | "ocamlopt" | "ocamlopt.opt" ->
  >      let self_dir = Filename.dirname Sys.executable_name in
  >      let stdlib = Filename.concat self_dir "../lib/ocaml" in
  >      Unix.putenv "OCAMLLIB" stdlib
  >    | _ -> ());
  >   Unix.execv (Filename.concat real_ocaml_bin tool) Sys.argv
  > EOF
  > } > fake-compiler/compiler.ml
  $ ocamlopt -I +unix unix.cmxa fake-compiler/compiler.ml \
  >   -o fake-compiler/compiler
  $ rm -f fake-compiler/compiler.cmi fake-compiler/compiler.cmx
  $ rm -f fake-compiler/compiler.o

  $ mkdir fake-menhir
  $ {
  >   echo '#!/bin/sh'
  >   echo "exec '$real_menhir' \"\$@\""
  > } > fake-menhir/menhir
  $ chmod +x fake-menhir/menhir

The marker package makes Dune install the compiler as a regular package rather
than through the non-relocatable toolchain mechanism.

  $ make_lockdir
  $ cat >> dune.lock/lock.dune <<'EOF'
  > (ocaml ocaml-base-compiler)
  > EOF

  $ make_lockpkg relocatable-compiler <<EOF
  > (version $ocaml_version)
  > (install
  >  (progn
  >   (run mkdir -p %{prefix}/bin %{prefix}/lib)
  >   (run cp compiler %{prefix}/bin/ocaml)
  >   (run cp compiler %{prefix}/bin/ocamlc)
  >   (run cp compiler %{prefix}/bin/ocamlc.opt)
  >   (run cp compiler %{prefix}/bin/ocamldep)
  >   (run cp compiler %{prefix}/bin/ocamldep.opt)
  >   (run cp compiler %{prefix}/bin/ocamlmklib)
  >   (run cp compiler %{prefix}/bin/ocamlobjinfo)
  >   (run cp compiler %{prefix}/bin/ocamlopt)
  >   (run cp compiler %{prefix}/bin/ocamlopt.opt)
  >   (run mkdir -p %{prefix}/lib/ocaml)
  >   (run cp $real_ocaml_lib/Makefile.config
  >    %{prefix}/lib/ocaml/Makefile.config)
  >   (run sh -c
  >    "cp $real_ocaml_lib/*.cmi %{prefix}/lib/ocaml")))
  > (source (copy $PWD/fake-compiler))
  > EOF

  $ make_lockpkg ocaml-compiler <<EOF
  > (version $ocaml_version)
  > (depends relocatable-compiler)
  > EOF

  $ make_lockpkg ocaml-base-compiler <<EOF
  > (version $ocaml_version)
  > (depends ocaml-compiler)
  > EOF

  $ make_lockpkg ocaml <<EOF
  > (version $ocaml_version)
  > (depends ocaml-base-compiler)
  > EOF

  $ make_lockpkg menhir <<EOF
  > (version 1)
  > (install
  >  (progn
  >   (run mkdir -p %{prefix}/bin)
  >   (run cp menhir %{prefix}/bin/menhir)))
  > (source (copy $PWD/fake-menhir))
  > EOF

  $ cat > dune-project <<'EOF'
  > (lang dune 3.24)
  > (using menhir 2.0)
  > (package
  >  (name repro)
  >  (allow_empty)
  >  (depends ocaml menhir))
  > EOF

  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.24)
  > (pkg enabled)
  > (context default)
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

  $ dune build _build/default/parser__mock.mli.inferred
  $ dune trace cat | jq -r '
  >   select(.cat == "process" and .name == "finish")
  >   | select(.args.prog | test("/ocamlc(\\.opt)?$"))
  >   | select(.args.process_args | index("parser__mock.ml.mock"))
  >   | .args.exit
  > '
  0
