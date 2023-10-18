Create empty opam repo
  $ mkdir -p mock-opam-repository/packages
  $ cat >mock-opam-repository/repo <<EOF
  > opam-version: "2.0"
  > EOF

Define several build contexts that all use the default lockdir
  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (context
  >  (default))
  > (context
  >  (default
  >   (name custom-context-with-default-lock-dir)))
  > EOF

Check that we can still generate lockdirs for individual contexts:
  $ dune pkg lock --opam-repository-path=mock-opam-repository
  Solution for dune.lock:
  (no dependencies to lock)
  $ dune pkg lock --opam-repository-path=mock-opam-repository --context=default
  Solution for dune.lock:
  (no dependencies to lock)
  $ dune pkg lock --opam-repository-path=mock-opam-repository --context=custom-context-with-default-lock-dir
  Solution for dune.lock:
  (no dependencies to lock)

It's an error to use --all-contexts when there are multiple contexts with the same lockdir:
  $ dune pkg lock --opam-repository-path=mock-opam-repository --all-contexts
  File "dune-workspace", line 5, characters 1-56:
  5 |  (default
  6 |   (name custom-context-with-default-lock-dir)))
  Error: Refusing to proceed as multiple selected contexts would create a lock
  dir at the same path.
  These contexts all create a lock dir: dune.lock
  - custom-context-with-default-lock-dir (defined at dune-workspace:5)
  - default (defined at dune-workspace:3)
  [1]

Define several build contexts that all use the same custom lockdir:
  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (context
  >  (default
  >   (name b)
  >   (lock foo.lock)))
  > (context
  >  (default
  >   (name a)
  >   (lock foo.lock)))
  > EOF

Check that we can still generate lockdirs for individual contexts:
  $ dune pkg lock --opam-repository-path=mock-opam-repository --context=a
  Solution for foo.lock:
  (no dependencies to lock)
  $ dune pkg lock --opam-repository-path=mock-opam-repository --context=b
  Solution for foo.lock:
  (no dependencies to lock)

It's an error to use --all-contexts when there are multiple contexts with the same lockdir:
  $ dune pkg lock --opam-repository-path=mock-opam-repository --all-contexts
  File "dune-workspace", line 7, characters 1-39:
  7 |  (default
  8 |   (name a)
  9 |   (lock foo.lock)))
  Error: Refusing to proceed as multiple selected contexts would create a lock
  dir at the same path.
  These contexts all create a lock dir: foo.lock
  - a (defined at dune-workspace:7)
  - b (defined at dune-workspace:3)
  [1]


