A compiler-integrated tool that has no explicit opam dependency on OCaml must
still be built with the project's compiler. Bake a value supplied by the
compiler into the installed tool. Changing only the project recipe (not the
version or the tool repository) distinguishes using the target from re-solving
for an independently built compiler of the same version.

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

The tool lock can be reused while the solver choices are unchanged.

  $ touch "$dev_tool_lock_dir/relock-sentinel"
  $ run_merlin
  no-compiler
  $ test -e "$dev_tool_lock_dir/relock-sentinel"
  [1]

Use an authoritative edit that cannot be obtained by re-solving the tool.

  $ set -- dune.lock/ocaml-base-compiler*.pkg
  $ dune_cmd subst compiler-v1 compiler-v2 <"$1" >updated
  $ mv updated "$1"
  $ run_merlin
  no-compiler
