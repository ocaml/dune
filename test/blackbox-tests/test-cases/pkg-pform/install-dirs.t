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

A file inside the installed directory cannot be referenced with the
descent path:

  $ cat >dune <<EOF
  > (rule
  >  (alias file-inside)
  >  (action (echo "%{pkg:foo:share:generated/a.txt}\n")))
  > EOF

  $ dune build @file-inside 2>&1
  File "dune", line 3, characters 16-48:
  3 |  (action (echo "%{pkg:foo:share:generated/a.txt}\n")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: File generated/a.txt not found in section share of package foo.
  [1]

Nested descent likewise fails:

  $ cat >dune <<EOF
  > (rule
  >  (alias nested-inside)
  >  (action (echo "%{pkg:foo:share:generated/sub/b.txt}\n")))
  > EOF

  $ dune build @nested-inside 2>&1
  File "dune", line 3, characters 16-52:
  3 |  (action (echo "%{pkg:foo:share:generated/sub/b.txt}\n")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: File generated/sub/b.txt not found in section share of package foo.
  [1]

The directory contents are reachable by appending to the dir-path result
(this is the workaround consumers use today):

  $ cat >dune <<EOF
  > (rule
  >  (alias append-path)
  >  (action (bash "cat %{pkg:foo:share:generated}/a.txt")))
  > EOF

  $ dune build @append-path 2>&1
  top
