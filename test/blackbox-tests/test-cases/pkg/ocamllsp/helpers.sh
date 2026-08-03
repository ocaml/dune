export dev_tool_lock_dir="_build/.dev-tools.locks/ocaml-lsp-server"

# Create a dune-workspace file with mock repos set up for the main
# project and the ocamllsp lockdir.
setup_ocamllsp_workspace() {
  setup_dev_tool_workspace
}

# Create a fake ocaml-lsp-server package containing an executable that
# just prints a message.
make_mock_ocamllsp_package() {
  make_mock_dev_tool_package ocaml-lsp-server ocamllsp \
    "hello from fake ocamllsp"
}
