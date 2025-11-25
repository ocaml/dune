dev_tool_lock_dir="_build/.dev-tools.locks/ocaml-lsp-server"

# Create a dune-workspace file with mock repos set up for the main
# project and the ocamllsp lockdir.
setup_ocamllsp_workspace() {
  cat > dune-workspace <<EOF
(lang dune 3.20)
(lock_dir
 (path "${dev_tool_lock_dir}")
 (repositories mock))
 (lock_dir
  (repositories mock))
(repository
 (name mock)
 (url "file://$(pwd)/mock-opam-repository"))
(pkg enabled)
EOF
}

# Create a fake ocaml-lsp-server package containing an executable that
# just prints a message.
make_mock_ocamllsp_package() {
  mkpkg ocaml-lsp-server <<EOF
install: [
  [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/ocamllsp" ]
  [ "sh" "-c" "echo 'echo hello from fake ocamllsp' >> %{bin}%/ocamllsp" ]
  [ "sh" "-c" "chmod a+x %{bin}%/ocamllsp" ]
]
EOF
}
