Test `dune describe pkg lock` with autolocking (no committed lock dir).

  $ mkrepo
  $ mkpkg A 1.2.0
  > mkpkg B 2.1+rc1

  $ add_mock_repo_if_needed
  $ enable_pkg

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package
  >  (name x)
  >  (depends A B))
  > EOF

There is no committed lock dir, so describe autolocks on demand (building only
the lock dir) and shows the generated lock:
  $ dune describe pkg lock
  Contents of _build/_private/default/.lock/dune.lock:
  - A.1.2.0
  - B.2.1+rc1

A subsequent build reuses the same autolocked lock dir, which describe still
reads:
  $ dune build @pkg-install 2>&1
  $ dune describe pkg lock
  Contents of _build/_private/default/.lock/dune.lock:
  - A.1.2.0
  - B.2.1+rc1
