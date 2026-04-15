Test error handling for %{pkg:...} pforms.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name foo))
  > EOF

  $ mkdir -p foo
  $ cat >foo/dune <<EOF
  > (install (section share) (package foo) (files (src.txt as dest.txt)))
  > EOF
  $ cat >foo/src.txt <<EOF
  > some data
  > EOF

CR-soon alizter: Section and argument count errors could be validated eagerly
at pform parse time (in Pform.Env.parse) so they fire even when the rule is
not demanded. Currently all %{pkg:..} errors are lazy.

All %{pkg:..} errors are lazy and do not prevent building unrelated targets in
the same directory. Each case below includes an unrelated rule to verify this.

Non-existent packages are rejected:

  $ cat >dune <<EOF
  > (rule
  >  (alias test-bad)
  >  (action (echo "%{pkg:nonexistent:share:dest.txt}\n")))
  > (rule
  >  (with-stdout-to ok.txt (echo "hello")))
  > EOF

  $ dune build ok.txt 2>&1
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
  > (rule
  >  (with-stdout-to ok2.txt (echo "hello")))
  > EOF

  $ dune build ok2.txt 2>&1
  $ dune build @test-bad-file 2>&1
  File "dune", line 3, characters 16-44:
  3 |  (action (echo "%{pkg:foo:share:missing.txt}\n")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: File missing.txt not found in section share of package foo.
  [1]

A typo in the filename shows a hint:

  $ cat >dune <<EOF
  > (rule
  >  (alias test-typo)
  >  (action (echo "%{pkg:foo:share:dest.tx}\n")))
  > EOF

  $ dune build @test-typo 2>&1
  File "dune", line 3, characters 16-40:
  3 |  (action (echo "%{pkg:foo:share:dest.tx}\n")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: File dest.tx not found in section share of package foo.
  Hint: did you mean dest.txt?
  [1]

Invalid section name is rejected:

  $ cat >dune <<EOF
  > (rule
  >  (alias test-bad-section)
  >  (action (echo "%{pkg:foo:badsection:dest.txt}\n")))
  > (rule
  >  (with-stdout-to ok3.txt (echo "hello")))
  > EOF

  $ dune build ok3.txt 2>&1
  $ dune build @test-bad-section 2>&1
  File "dune", line 3, characters 16-46:
  3 |  (action (echo "%{pkg:foo:badsection:dest.txt}\n")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: badsection is not a valid section.
  Hint: supported sections are lib, lib_root, libexec, libexec_root, bin, sbin,
  toplevel, share, share_root, etc, doc, stublibs and man.
  [1]

The misc section is rejected:

  $ cat >dune <<EOF
  > (rule
  >  (alias test-misc)
  >  (action (echo "%{pkg:foo:misc:dest.txt}\n")))
  > EOF

  $ dune build @test-misc 2>&1
  File "dune", line 3, characters 16-40:
  3 |  (action (echo "%{pkg:foo:misc:dest.txt}\n")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: misc is not a valid section.
  Hint: supported sections are lib, lib_root, libexec, libexec_root, bin, sbin,
  toplevel, share, share_root, etc, doc, stublibs and man.
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
  Error: %{pkg:..} requires exactly 3 arguments.
  Hint: the syntax is %{pkg:PACKAGE:SECTION:PATH}
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
  Error: %{pkg:..} requires exactly 3 arguments.
  Hint: the syntax is %{pkg:PACKAGE:SECTION:PATH}
  [1]

Path escaping the workspace is rejected:

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name foo))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias test-escape)
  >  (action (echo "%{pkg:foo:share:../../../etc/passwd}\n")))
  > EOF

  $ dune build @test-escape 2>&1
  File "dune", line 3, characters 16-52:
  3 |  (action (echo "%{pkg:foo:share:../../../etc/passwd}\n")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: ../../../etc/passwd is not a valid file path.
  Hint: the path must be relative and must not contain '..'.
  [1]

The %{pkg:...} macro requires (lang dune 3.24) or later:

  $ cat >dune-project <<EOF
  > (lang dune 3.23)
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
  Error: %{pkg:..} is only available since version 3.24 of the dune language.
  Please update your dune-project file to have (lang dune 3.24).
  [1]
