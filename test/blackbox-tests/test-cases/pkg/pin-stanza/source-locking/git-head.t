Pinning a git+file:// URL tracks the locked commit.

  $ setup_source_locking_project

  $ write_pin_project "git+file://$PWD/_dependency"
  $ write_dependency_version "git+file:// initial"

We should be getting the latest committed version from the git repo:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  initial

If we update the dependency and commit:

  $ write_dependency_version "git+file:// updated"
  $ commit_dependency "Update"

We should still be getting the initial message since the lock dir has not been
updated:

  $ dune exec ./main.exe
  initial

When we re-lock, we should lock the new revision of the dependency and build
that:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  git+file:// updated
