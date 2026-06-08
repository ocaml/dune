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

Before any build there is no lock dir, so describe reports none rather than
failing:
  $ dune describe pkg lock
  Error: dune.lock/lock.dune: No such file or directory
  [1]

Trigger autolocking with a build:
  $ dune build @pkg-install 2>&1

After the build, describe reads the internal autolocked lock:
  $ dune describe pkg lock
  Error: dune.lock/lock.dune: No such file or directory
  [1]
