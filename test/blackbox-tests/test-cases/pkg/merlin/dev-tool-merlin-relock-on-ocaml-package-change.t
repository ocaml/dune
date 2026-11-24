Record which compiler Merlin sees at install time when its opam package does
not depend on OCaml. Constraining the compiler version in the tool solve
does not put the project's installed compiler on the recipe's PATH.

  $ mkrepo
  $ mk_ocaml 5.5.0
  $ cat >>"$mock_packages/ocaml-base-compiler/ocaml-base-compiler.5.5.0/opam" <<'EOF'
  > install: [
  >   [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/compiler-id; echo 'echo compiler-v1' >> %{bin}%/compiler-id; chmod +x %{bin}%/compiler-id" ]
  > ]
  > EOF
  $ mkpkg merlin <<'EOF'
  > install: [
  >   [ "sh" "-c" "id=$(compiler-id 2>/dev/null || echo no-compiler); echo '#!/bin/sh' > %{bin}%/ocamlmerlin; echo echo $id >> %{bin}%/ocamlmerlin; chmod +x %{bin}%/ocamlmerlin" ]
  > ]
  > EOF
  $ setup_merlin_workspace
  $ make_named_package_project foo 3.16 "(ocaml (= 5.5.0))"
  $ export DUNE_CACHE=disabled
  $ dune pkg lock > /dev/null 2>&1
  $ run_merlin() {
  >   dune tools exec ocamlmerlin >log 2>&1 && tail -n 1 log
  > }
  $ run_merlin
  no-compiler

Check whether the existing tool lock is reused on a second invocation.

  $ touch "$dev_tool_lock_dir/relock-sentinel"
  $ run_merlin
  no-compiler
  $ test -e "$dev_tool_lock_dir/relock-sentinel"
  [1]

Changing only the project compiler's recipe does not make it visible to Merlin.

  $ set -- dune.lock/ocaml-base-compiler*.pkg
  $ dune_cmd subst compiler-v1 compiler-v2 <"$1" >updated
  $ mv updated "$1"
  $ run_merlin
  no-compiler

Now give Merlin an explicit compiler dependency and solve its lock again. The
installed project compiler reports v2, but Merlin builds with its own v1.

  $ build_pkg ocaml-base-compiler
  $ "$(get_build_pkg_dir ocaml-base-compiler)/target/bin/compiler-id"
  compiler-v2
  $ merlin_opam="$mock_packages/merlin/merlin.0.0.1/opam"
  $ echo 'depends: [ "ocaml-base-compiler" ]' >>"$merlin_opam"
  $ mv "$dev_tool_lock_dir" "$dev_tool_lock_dir.orig"
  $ run_merlin
  compiler-v1
