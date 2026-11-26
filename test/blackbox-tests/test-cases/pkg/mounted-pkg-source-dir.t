A workspace may contain a directory with the name used for mounted packages.
Its contents are not sources of the build.

  $ make_foo_consumer
  $ mkdir _source
  $ (cd _source && make_dune_project_with_package 3.25 foo)
  $ echo '(library (public_name foo))' > _source/dune
  $ echo 'let message = "mounted"' > _source/foo.ml
  $ make_dune_source_lockpkg native foo "$PWD/_source"

Files in the workspace directory, including ones that shadow the package's
files and a dune file with a failing rule.

  $ mkdir -p .pkg/foo .pkg/other
  $ echo 'let message = "workspace"' > .pkg/foo/foo.ml
  $ echo stray > .pkg/foo/stray
  $ echo '(rule (alias default) (action (run false)))' > .pkg/other/dune
  $ echo '(rule (alias default) (action (run false)))' > .pkg/dune

  $ dune build main.exe
  $ _build/default/main.exe
  mounted
  $ dune build
  $ test ! -e _build/default/.pkg/foo/stray
  $ test ! -e _build/default/.pkg/other
  $ dune build _build/default/.pkg/foo/stray
  Error: Don't know how to build _build/default/.pkg/foo/stray
  [1]
  $ dune build @_build/default/.pkg/other/default
  Error: Don't know about directory .pkg/other specified on the command line!
  [1]

Explicitly including the directory in the workspace is an error.

  $ echo '(dirs :standard .pkg)' >> dune
  $ dune build main.exe
  File ".pkg", line 1, characters 0-0:
  Error: The directory .pkg is reserved for locked packages and cannot be part
  of the workspace.
  [1]
