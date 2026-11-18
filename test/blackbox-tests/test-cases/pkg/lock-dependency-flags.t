Test the --with-test, --with-doc and --with-dev-setup flags of "dune pkg lock".
They override the corresponding variables of the solver environment.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg foo
  $ mkpkg test-dep
  $ mkpkg doc-dep
  $ mkpkg dev-setup-dep
  $ mkpkg transitive-test-dep
  $ mkpkg transitive-doc-dep
  $ mkpkg depends-on-flagged-deps <<EOF
  > depends: [
  >   "transitive-test-dep" {with-test}
  >   "transitive-doc-dep" {with-doc}
  > ]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends
  >   foo
  >   depends-on-flagged-deps
  >   (test-dep :with-test)
  >   (doc-dep :with-doc)
  >   (dev-setup-dep :with-dev-setup)))
  > EOF

By default, test dependencies of local packages are included but doc and
dev-setup dependencies are not:

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - depends-on-flagged-deps.0.0.1
  - foo.0.0.1
  - test-dep.0.0.1

Doc dependencies can be requested. Like the "with-doc" solver variable, this
applies to all packages, local or not:

  $ dune pkg lock --with-doc
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - depends-on-flagged-deps.0.0.1
  - doc-dep.0.0.1
  - foo.0.0.1
  - test-dep.0.0.1
  - transitive-doc-dep.0.0.1

The variable is recorded in the lock directory, so that it is still considered
valid:

  $ grep -A 1 variable_values ${default_lock_dir}/lock.dune
   (variable_values
    (with-doc true)

  $ dune pkg validate-lockdir

Dev-setup dependencies can be requested too, and test dependencies of local
packages can be excluded:

  $ dune pkg lock --with-dev-setup --with-test=false
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - depends-on-flagged-deps.0.0.1
  - dev-setup-dep.0.0.1
  - foo.0.0.1

  $ dune pkg validate-lockdir

Flags are not remembered when locking again:

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - depends-on-flagged-deps.0.0.1
  - foo.0.0.1
  - test-dep.0.0.1

The flags override the solver environment of the workspace:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.21)
  > (lock_dir
  >  (repositories mock)
  >  (solver_env
  >   (with-test false)
  >   (with-doc true)))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - depends-on-flagged-deps.0.0.1
  - doc-dep.0.0.1
  - foo.0.0.1
  - transitive-doc-dep.0.0.1

  $ dune pkg lock --with-test --with-doc=false
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - depends-on-flagged-deps.0.0.1
  - foo.0.0.1
  - test-dep.0.0.1

The same holds for portable lock directories, where the workspace solver
environment is also used for each platform:

  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock --with-test --with-doc=false
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - depends-on-flagged-deps.0.0.1
  - foo.0.0.1
  - test-dep.0.0.1

The flags also override variables unset in the workspace:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.21)
  > (lock_dir
  >  (repositories mock)
  >  (unset_solver_vars with-doc))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ dune pkg lock --with-doc
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - depends-on-flagged-deps.0.0.1
  - doc-dep.0.0.1
  - foo.0.0.1
  - test-dep.0.0.1
  - transitive-doc-dep.0.0.1
