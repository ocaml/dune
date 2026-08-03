export dev_tool_lock_dir="_build/.dev-tools.locks/odoc"

# Create a dune-workspace file with mock repos set up for the main
# project and the odoc lockdir.
setup_odoc_workspace() {
  setup_dev_tool_workspace
}

# Create a fake odoc package containing an executable that
# just prints a message.
make_mock_odoc_package() {
  make_mock_dev_tool_package odoc odoc "hello from fake odoc"
}
