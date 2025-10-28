Demonstrate how depopts can be forced in the workspace

  $ . ../helpers.sh

  $ mkrepo

  $ mkpkg foo
  $ mkpkg bar
  $ mkpkg baz

  $ solve_project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depopts foo bar))
  > EOF
  Solution for dune.lock:
  (no dependencies to lock)

Select just foo

  $ cat >dune-workspace <<EOF
  > (lang dune 3.10)
  > (lock_dir
  >  (depopts foo)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$PWD/mock-opam-repository"))
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - foo.0.0.1

Select both foo and bar

  $ cat >dune-workspace <<EOF
  > (lang dune 3.10)
  > (lock_dir
  >  (depopts foo bar)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$PWD/mock-opam-repository"))
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - bar.0.0.1
  - foo.0.0.1

Select a package that is not listed as depopt

  $ cat >dune-workspace <<EOF
  > (lang dune 3.10)
  > (lock_dir
  >  (depopts baz)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$PWD/mock-opam-repository"))
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  (no dependencies to lock)


Select garbage

  $ cat >dune-workspace <<EOF
  > (lang dune 3.10)
  > (lock_dir
  >  (depopts z)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$PWD/mock-opam-repository"))
  > EOF

  $ dune_pkg_lock_normalized
  Error:
  Unable to solve dependencies while generating lock directory: dune.lock
  
  Couldn't solve the package dependency formula.
  The following packages couldn't be found: z
  [1]
