Test `dune describe pkg lock` with autolocking (no committed lock dir).

  $ mkrepo
  $ mkpkg A 1.2.0
  > mkpkg B 2.1+rc1

  $ add_mock_repo_if_needed

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package
  >  (name x)
  >  (depends A B))
  > EOF

When package management is not enabled, describe does not autolock: with no
committed lock dir it reports none rather than running the solver:
  $ dune describe pkg lock
  No lock directory found for context default (package management is not
  enabled and no lock directory is committed).

  $ enable_pkg

There is no committed lock dir, so describe autolocks on demand (building only
the lock dir) and shows the generated lock:
  $ dune describe pkg lock
  Solution for _build/_private/default/.lock/dune.lock
  
  Dependencies common to all supported platforms:
  - A.1.2.0
  - B.2.1+rc1

A subsequent build reuses the same autolocked lock dir, which describe still
reads:
  $ dune build @pkg-install 2>&1
  $ dune describe pkg lock
  Solution for _build/_private/default/.lock/dune.lock
  
  Dependencies common to all supported platforms:
  - A.1.2.0
  - B.2.1+rc1

With multiple contexts, each context has its own lock dir. Add a second context
"foo" with its own lock dir, neither committed:
  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > (context
  >  (default))
  > (context
  >  (default
  >   (name foo)
  >   (lock_dir foo.lock)))
  > (lock_dir
  >  (repositories mock))
  > (lock_dir
  >  (path foo.lock)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

The default context autolocks its own lock dir:
  $ dune describe pkg lock
  Solution for _build/_private/default/.lock/dune.lock
  
  Dependencies common to all supported platforms:
  - A.1.2.0
  - B.2.1+rc1

Another context's lock dir is autolocked and selected with --context:
  $ dune describe pkg lock --context foo
  Solution for _build/_private/default/.lock/foo.lock
  
  Dependencies common to all supported platforms:
  - A.1.2.0
  - B.2.1+rc1

