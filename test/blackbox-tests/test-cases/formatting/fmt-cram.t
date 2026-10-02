A test showing that `@fmt` loads more rules than it needs to for formatting.

  $ make_dune_project 3.0

We add a cram rule which requires a specific package to be installed

  $ cat > dune << EOF
  > (cram
  >  (applies_to *)
  >  (enabled_if
  >   (>= %{version:not-installed-pkg} 1.0)))
  > EOF

We add a test. Without the test no rules are created and the issue is not appearing.

  $ cat > some-test.t << EOF
  > A cram test which fails (so there's output)
  > 
  > $ not-installed-package --api-version=1.0
  > EOF

Formatting attempts to evaluate the pform and fails. However there is no need
to evaluate the pform (at least not in `cram` stanzas) for `@fmt`.

  $ dune build @fmt
  File "dune", line 4, characters 6-34:
  4 |   (>= %{version:not-installed-pkg} 1.0)))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Package "not-installed-pkg" doesn't exist in the current project and
  isn't installed either.
  [1]
