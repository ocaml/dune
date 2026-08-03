export dev_tool_lock_dir="_build/.dev-tools.locks/merlin"

# Create a dune-workspace file with mock repos configured for the main
# project lockdir and the merlin dev-tool lockdir.
setup_merlin_workspace() {
  setup_dev_tool_workspace
}

# Create a fake merlin package containing an executable that
# just prints a message.
make_mock_merlin_package() {
  make_mock_dev_tool_package merlin ocamlmerlin \
    "hello from fake ocamlmerlin"
}
