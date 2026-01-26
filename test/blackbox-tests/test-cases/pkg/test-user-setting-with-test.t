Test that setting the "with-test" variable in solver_env works as expected.

Add a workspace that sets the "with-test" variable to "true":
  $ mkrepo
  $ cat >dune-workspace <<EOF
  > (lang dune 3.21)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ mkpkg foo
  $ mkpkg bar

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > 
  > (package
  >  (name foo-pack)
  >  (allow_empty)
  >  (depends
  >   foo
  >   (bar :with-test)))
  > EOF

Both packages will be added to the solution because the dune-workspace setting
explicitly tells to install test dependencies.

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - bar.0.0.1
  - foo.0.0.1

Setting the "with-test" variable to false.

  $ cat >dune-workspace <<EOF
  > (lang dune 3.21)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock)
  >  (solver_env
  >   (with-test false)))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF


  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > 
  > (package
  >  (name foo-pack)
  >  (allow_empty)
  >  (depends
  >   foo 
  >   (bar :with-test)))
  > EOF

  $ dune pkg print-solver-env 2>&1 | grep with-test
  - with-test = false

Now on the contrary, setting the "with-test" variable to false will not include
test dependencies in the solution.

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - foo.0.0.1

Setting the value of with-test to true would fail:

  $ cat >dune-workspace <<EOF
  > (lang dune 3.21)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock)
  >  (solver_env
  >   (with-test true)))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ dune pkg lock
  File "dune-workspace", line 6, characters 2-18:
  6 |   (with-test true)))
        ^^^^^^^^^^^^^^^^
  Error: Setting the "with-test" solver variable to 'true' is not currently
  supported. Dune already uses "with-test" for local packages, but it cannot be
  applied to transitive dependencies.
  [1]
Now change workspace to with-test to default value again:

  $ cat >dune-workspace <<EOF
  > (lang dune 3.21)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

The env variable is not longer set to false:

  $ dune pkg print-solver-env 2>&1 | grep with-test
  [1]

However, validate-lockdir and @pkg-install don't catch the change:

  $ dune pkg validate-lockdir
  $ dune build @pkg-install

But when you re-lock it, you see a different solution
  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - bar.0.0.1
  - foo.0.0.1
