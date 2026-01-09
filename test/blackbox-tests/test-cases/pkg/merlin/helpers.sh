dev_tool_lock_dir="_build/.dev-tools.locks/merlin"

# Create a dune-workspace file with mock repos configured for the main
# project lockdir and the merlin dev-tool lockdir.
setup_merlin_workspace() {
  cat > dune-workspace <<- EOF
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

# Create a fake merlin package containing an executable that
# just prints a message.
make_mock_merlin_package() {
  mkpkg merlin <<- EOF
	install: [
	  [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/ocamlmerlin" ]
	  [ "sh" "-c" "echo 'echo hello from fake ocamlmerlin' >> %{bin}%/ocamlmerlin" ]
	  [ "sh" "-c" "chmod a+x %{bin}%/ocamlmerlin" ]
	]
	EOF
}
