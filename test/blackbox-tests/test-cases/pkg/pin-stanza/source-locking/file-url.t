Pinning a file:// URL tracks the dependency worktree.

  $ setup_source_locking_project

  $ write_pin_project "file://$PWD/_dependency"
  $ write_dependency_version "file:// initial"

We should be getting the value that the work dir currently has:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  file:// initial

When we update the work dir we should be getting the new content:

  $ write_dependency_version "file:// updated"
  $ dune exec ./main.exe
  file:// updated
