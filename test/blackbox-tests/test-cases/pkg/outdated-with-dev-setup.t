Reproduce internal error with dune pkg outdated in #11188.

  $ mkrepo
  $ mkpkg a
  $ mkpkg b

  $ solve_project<<EOF
  > (lang dune 3.20)
  > (package
  >  (name foo)
  >  (depends a (b :with-dev-setup)))
  > EOF
  Solution for dune.lock:
  - a.0.0.1

dune pkg outdated is able to handle :with-dev-setup correctly.
  $ dune pkg outdated
  dune.lock is up to date.

Add an updated version of b so it can be flagged as outdated
  $ mkpkg a 0.0.2
  $ mkpkg b 0.0.2

Now update the solver env to include with-dev-setup deps
  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (lock_dir
  >  (repositories mock)
  >  (solver_env
  >   (with-dev-setup true)))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

Show that the outdated b is not reported, since we are failing to take the
configured :with-dev-setup into account.
  $ dune pkg outdated
  1/1 packages in dune.lock are outdated.
  - a 0.0.1 < 0.0.2

