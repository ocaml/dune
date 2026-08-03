Testing whether the revision store locks properly.

To start with we create a repository in with a `foo` package.

  $ make_committed_mock_repo_package foo 1.0

We set this repository as sole source for opam repositories.

  $ add_mock_repo_if_needed

We set the project up to depend on `foo`

  $ make_bar_depends_foo_project

Creating a lock should thus work.

  $ mkdir dune-workspace-cache
  $ XDG_CACHE_HOME=$(pwd)/fake-xdg-cache dune_pkg_lock_normalized
  Solution for dune.lock:
  - foo.1.0
