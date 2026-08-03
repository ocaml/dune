Exercises end to end locking and building a simple project.

  $ mkrepo
  $ add_mock_repo_if_needed

Make a library:
  $ make_foo_tarball 'let foo = "Hello, World!"'

Configure our fake curl to serve the tarball

  $ echo foo.tar >> fake-curls
  $ PORT=1

Make a package for the library:
  $ make_foo_tarball_package

Make a project that uses the library:

  $ make_bar_executable_depends_foo_project

Lock, build, and run the executable in the project:

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - foo.0.0.1
  $ dune exec bar
  Hello, World!
