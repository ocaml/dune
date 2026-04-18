Test error handling for %{pkg:...} pforms.

  $ cat >dune-project <<EOF
  > (lang dune 3.23)
  > (package (name foo))
  > EOF

  $ mkdir -p foo
  $ cat >foo/dune <<EOF
  > (install (section share) (package foo) (files (src.txt as dest.txt)))
  > EOF
  $ cat >foo/src.txt <<EOF
  > some data
  > EOF

Non-existent packages are rejected:

  $ cat >dune <<EOF
  > (rule
  >  (alias test-bad)
  >  (action (echo "%{pkg:nonexistent:share:dest.txt}\n")))
  > EOF

  $ dune build @test-bad 2>&1
  File "dune", line 3, characters 16-49:
  3 |  (action (echo "%{pkg:nonexistent:share:dest.txt}\n")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Package nonexistent does not exist
  [1]

File not found in a valid package is rejected:

  $ cat >dune <<EOF
  > (rule
  >  (alias test-bad-file)
  >  (action (echo "%{pkg:foo:share:missing.txt}\n")))
  > EOF

  $ dune build @test-bad-file 2>&1
  File "dune", line 3, characters 16-44:
  3 |  (action (echo "%{pkg:foo:share:missing.txt}\n")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: File missing.txt not found in section share of package foo
  [1]

Invalid section name is rejected:

  $ cat >dune <<EOF
  > (rule
  >  (alias test-bad-section)
  >  (action (echo "%{pkg:foo:badsection:dest.txt}\n")))
  > EOF

  $ dune build @test-bad-section 2>&1
  File "dune", line 3, characters 16-46:
  3 |  (action (echo "%{pkg:foo:badsection:dest.txt}\n")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: %{pkg:foo:badsection:dest.txt}: badsection is not a valid section;
  supported sections are lib, bin, share, etc.
  [1]

Wrong number of arguments (too few):

  $ cat >dune <<EOF
  > (rule
  >  (alias test-too-few)
  >  (action (echo "%{pkg:foo:share}\n")))
  > EOF

  $ dune build @test-too-few 2>&1
  File "dune", line 3, characters 16-32:
  3 |  (action (echo "%{pkg:foo:share}\n")))
                      ^^^^^^^^^^^^^^^^
  Error: %{pkg:..} requires 3 arguments: %{pkg:PACKAGE:SECTION:PATH}
  [1]

Wrong number of arguments (too many):

  $ cat >dune <<EOF
  > (rule
  >  (alias test-too-many)
  >  (action (echo "%{pkg:foo:share:dest.txt:extra}\n")))
  > EOF

  $ dune build @test-too-many 2>&1
  File "dune", line 3, characters 16-47:
  3 |  (action (echo "%{pkg:foo:share:dest.txt:extra}\n")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: %{pkg:..} requires 3 arguments: %{pkg:PACKAGE:SECTION:PATH}
  [1]

The %{pkg:...} macro requires (lang dune 3.23) or later:

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (package (name foo))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias test-version-gate)
  >  (action (echo "%{pkg:foo:share:dest.txt}\n")))
  > EOF

  $ dune build @test-version-gate 2>&1
  File "dune", line 3, characters 16-41:
  3 |  (action (echo "%{pkg:foo:share:dest.txt}\n")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: %{pkg:..} is only available since version 3.23 of the dune language.
  Please update your dune-project file to have (lang dune 3.23).
  [1]
