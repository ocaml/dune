Test that `dune pkg lock` (with no arguments) respects the `(lock_dir (path ...))`
stanza in dune-workspace, rather than always defaulting to dune.lock.

See https://github.com/ocaml/dune/issues/13841

  $ mkrepo
  $ mkpkg foo

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (package
  >  (name test)
  >  (depends foo))
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.21)
  > 
  > (lock_dir
  >  (path foo.lock)
  >  (repositories mock))
  > 
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

Running `dune pkg lock` with no arguments uses the path from the `lock_dir`
stanza:

  $ dune pkg lock
  Solution for foo.lock
  
  Dependencies common to all supported platforms:
  - foo.0.0.1

Here foo.lock is created, not dune.lock

  $ find . -maxdepth 1 -name "*.lock"
  ./foo.lock

Running with an explicit argument matching the configured path:

  $ rm -rf foo.lock
  $ dune pkg lock foo.lock
  Solution for foo.lock
  
  Dependencies common to all supported platforms:
  - foo.0.0.1

Now foo.lock is created

  $ find . -maxdepth 1 -name "*.lock"
  ./foo.lock

Passing an undeclared lock directory gives an error:

  $ dune pkg lock bar.lock
  Error: The following directories are not lock directories in this workspace:
  - bar.lock
  This workspace contains the following lock directories:
  - dune.lock
  - foo.lock
  [1]
