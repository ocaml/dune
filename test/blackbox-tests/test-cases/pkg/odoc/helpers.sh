dev_tool_lock_dir="dev-tools.locks/odoc"

# Create a dune-workspace file with mock repos set up for the main
# project and the odoc lockdir.
setup_odoc_workspace() {
  cat > dune-workspace <<EOF
(lang dune 3.20)
(pkg enabled)
(lock_dir
 (path "${dev_tool_lock_dir}")
 (repositories mock))
 (lock_dir
  (repositories mock))
(repository
 (name mock)
 (url "file://$(pwd)/mock-opam-repository"))
EOF
}

# Create a fake odoc package containing an executable that
# just prints a message.
make_mock_odoc_package() {
  mkpkg odoc <<EOF
install: [
  [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/odoc" ]
  [ "sh" "-c" "echo 'echo hello from fake odoc' >> %{bin}%/odoc" ]
  [ "sh" "-c" "chmod a+x %{bin}%/odoc" ]
]
EOF
}
