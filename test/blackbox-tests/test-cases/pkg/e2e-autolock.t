Same setup as e2e.t but this time using building without an explicit
`dune pkg lock`.

  $ mkrepo
  $ add_mock_repo_if_needed
  $ enable_pkg

Make a library:
  $ make_foo_tarball 'let foo = "Hello, World!"'

Configure our fake curl to serve the tarball

  $ echo foo.tar >> fake-curls
  $ PORT=1

Make a package for the library:
  $ make_foo_tarball_package

Make a project that uses the library:

  $ make_bar_executable_depends_foo_project

Lock, build, and run the executable in the project (without dune pkg lock):

  $ dune exec bar
  Hello, World!

