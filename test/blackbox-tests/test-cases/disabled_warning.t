Test detection and disabling of common typos in package dependencies

First, create a dune-project file with typos to see warnings with default settings (warnings enabled)
  $ cat > dune-project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >   (bar (= version))  ; Should detect typo and suggest :version
  >   (baz :with_test)   ; Should detect typo and suggest :with-test
  >   qux))
  > EOF

  $ dune pkg lock
  File "dune-project", line 6, characters 3-20:
  6 |    (bar (= version))  ; Should detect typo and suggest :version
         ^^^^^^^^^^^^^^^^^
  Warning: Possible typo in constraint for dependency "bar": '(= version)' might be a mistake.
  Hint: Did you mean to use the `:version` variable instead? Example: (depends (bar (= :version)))
  File "dune-project", line 7, characters 3-19:
  7 |    (baz :with_test)   ; Should detect typo and suggest :with-test
         ^^^^^^^^^^^^^^^^
  Warning: Possible typo in constraint for dependency "baz": ':with_test' might be a mistake.
  Hint: Did you mean to use ':with-test' instead? Example: (depends (baz :with-test))

Now demonstrate that fixed dependencies don't cause warnings
  $ cat > fixed-dune-project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >   (bar (= :version))  ; Fixed version
  >   (baz :with-test)    ; Fixed with-test
  >   qux))
  > EOF

  $ dune pkg lock --project-file=fixed-dune-project

Now test that we can disable warnings via the environment variable
  $ DUNE_CONFIG__TYPO_WARNINGS=disabled dune pkg lock

  $ # Create clean directory for next test
  $ mkdir disable_test
  $ cd disable_test

  $ cat > dune-project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >   (bar (= version))  ; Should NOT show warning with typo_warnings disabled
  >   (baz :with_test)   ; Should NOT show warning with typo_warnings disabled
  >   qux))
  > EOF

  $ DUNE_CONFIG__TYPO_WARNINGS=disabled dune pkg lock
  $ # No warnings should be produced when the feature is disabled