Observe `%{pkg:...}` behaviour against an `(install (dirs ...))` entry
for a built directory target.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name foo))
  > EOF

  $ mkdir src
  $ cat >src/dune <<EOF
  > (rule
  >  (target (dir generated))
  >  (action
  >   (progn (system "mkdir %{target}")
  >          (system "echo top > %{target}/a.txt")
  >          (system "mkdir %{target}/sub")
  >          (system "echo nested > %{target}/sub/b.txt"))))
  > (install (section share) (package foo) (dirs generated))
  > EOF

The directory entry itself resolves to its source path:

  $ cat >dune <<EOF
  > (rule
  >  (alias dir-path)
  >  (action (echo "%{pkg:foo:share:generated}\n")))
  > EOF

  $ dune build @dir-path 2>&1
  src/generated

A file inside the installed directory resolves by descent:

  $ cat >dune <<EOF
  > (rule
  >  (alias file-inside)
  >  (action (echo "%{pkg:foo:share:generated/a.txt}\n")))
  > EOF

  $ dune build @file-inside 2>&1
  src/generated/a.txt

Nested descent works too:

  $ cat >dune <<EOF
  > (rule
  >  (alias nested-inside)
  >  (action (echo "%{pkg:foo:share:generated/sub/b.txt}\n")))
  > EOF

  $ dune build @nested-inside 2>&1
  src/generated/sub/b.txt

Appending to the dir-path result also works (the pre-existing pattern):

  $ cat >dune <<EOF
  > (rule
  >  (alias append-path)
  >  (action (bash "cat %{pkg:foo:share:generated}/a.txt")))
  > EOF

  $ dune build @append-path 2>&1
  top

A path that's not inside the installed directory errors:

  $ cat >dune <<EOF
  > (rule
  >  (alias missing)
  >  (action (echo "%{pkg:foo:share:generated/missing.txt}\n")))
  > EOF

  $ dune build @missing 2>&1
  File "dune", line 3, characters 16-54:
  3 |  (action (echo "%{pkg:foo:share:generated/missing.txt}\n")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: File missing.txt not found inside directory generated installed by
  package foo.
  [1]
