Test error handling for %{pkg:...} pforms.

  $ cat >dune-project <<EOF
  > (lang dune 3.23)
  > (package (name foo))
  > EOF

  $ mkdir -p foo
  $ cat >foo/dune <<EOF
  > (install (section share) (package foo) (files (data.txt as data.txt)))
  > EOF
  $ cat >foo/data.txt <<EOF
  > some data
  > EOF

Non-existent packages are rejected:

  $ cat >dune <<EOF
  > (rule
  >  (alias test-bad)
  >  (action (system "echo %{pkg:nonexistent:lib}")))
  > EOF

  $ dune build @test-bad 2>&1
  File "dune", line 3, characters 23-45:
  3 |  (action (system "echo %{pkg:nonexistent:lib}")))
                             ^^^^^^^^^^^^^^^^^^^^^^
  Error: Package nonexistent does not exist
  [1]

Unsupported variable names (non-section) are rejected:

  $ cat >dune <<EOF
  > (rule
  >  (alias test-bad-var)
  >  (action (system "echo %{pkg:foo:version}")))
  > EOF

  $ dune build @test-bad-var 2>&1
  File "dune", line 3, characters 23-41:
  3 |  (action (system "echo %{pkg:foo:version}")))
                             ^^^^^^^^^^^^^^^^^^
  Error: %{pkg:foo:version} is not supported; only section variables (lib, bin,
  share, etc.) are available in regular rules
  [1]

Malformed pform with missing section separator gives a clear error:

  $ cat >dune <<EOF
  > (rule
  >  (alias test-malformed)
  >  (action (system "echo %{pkg:foo}")))
  > EOF

  $ dune build @test-malformed 2>&1
  File "dune", line 3, characters 29-32:
  3 |  (action (system "echo %{pkg:foo}")))
                                   ^^^
  Incorrect arguments for macro pkg.
  Error: Expected two arguments separated by ':' but no ':' found.
  [1]

The %{pkg:...} macro requires (lang dune 3.23) or later:

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (package (name foo))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias test-version-gate)
  >  (action (system "echo %{pkg:foo:lib}")))
  > EOF

  $ dune build @test-version-gate 2>&1
  File "dune", line 3, characters 23-37:
  3 |  (action (system "echo %{pkg:foo:lib}")))
                             ^^^^^^^^^^^^^^
  Error: %{pkg:..} is only available since version 3.23 of the dune language.
  Please update your dune-project file to have (lang dune 3.23).
  [1]
