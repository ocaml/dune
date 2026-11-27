Editing a lock file changes how its package is built without leaving stale
results behind.

  $ make_foo_consumer
  $ mkdir _source
  $ (cd _source && make_dune_project_with_package 3.25 foo)
  $ echo '(library (public_name foo))' > _source/dune
  $ echo 'let message = "native"' > _source/foo.ml
  $ echo 'unused' > _source/template.in
  $ lock() {
  >   echo "(version $1) (source (copy $PWD/_source)) (build (run false))" |
  >     make_lockpkg foo; }
  $ lock 1.0

The package is a Dune project, so it is built natively and its recipe does not
run.

  $ dune build main.exe @pkg-install
  $ _build/default/main.exe
  native
  $ find _build/default -name META.foo -exec grep '^version' {} \;
  version = "1.0"

A new version in the lock file reaches the generated metadata.

  $ lock 2.0
  $ dune build main.exe @pkg-install
  $ find _build/default -name META.foo -exec grep '^version' {} \;
  version = "2.0"

Adding a substitution makes the package opaque: its recipe now runs.

  $ make_lockpkg foo <<EOF
  > (version 2.0)
  > (source (copy $PWD/_source))
  > (build
  >  (progn
  >   (substitute template.in template)
  >   (run false)))
  > EOF
  $ dune build main.exe 2>&1 | grep -v '^ *$'
  File "dune.lock/foo.pkg", line 6, characters 7-12:
  6 |   (run false)))
             ^^^^^
  Error: Logs for package foo
  [1]

The natively generated metadata is gone.

  $ find _build/default -name META.foo

Removing it again returns to the native build.

  $ lock 2.0
  $ dune build main.exe @pkg-install
  $ _build/default/main.exe
  native
  $ find _build/default -name META.foo -exec grep '^version' {} \;
  version = "2.0"
