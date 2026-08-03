Pinning a local path tracks the dependency worktree.

  $ setup_source_locking_project

  $ write_pin_project "$PWD/_dependency"

This should work and display the initial value:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  initial

Updating the pinned dependency should update the output:

  $ write_dependency_version updated
  $ dune exec ./main.exe
  updated
