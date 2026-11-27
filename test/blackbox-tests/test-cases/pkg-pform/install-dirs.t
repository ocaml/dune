Observe `%{pkg:...}` behaviour against an `(install (dirs ...))` entry
for a built directory target.

  $ make_dune_project_with_package 3.24 foo

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

Use the same directory producer with locked providers. Rename its installation
to payload, and append paths to the directory pform rather than descending
inside it. Both top-level and nested files must be available to an action
with an explicit package dependency.

  $ mkdir _provider native opaque
  $ cp dune-project _provider/dune-project
  $ dune_cmd subst '\(dirs generated\)' '(dirs (generated as payload))' \
  >   < src/dune > _provider/dune
  $ cat > consumer-dune <<'EOF'
  > (rule
  >  (target directory-files)
  >  (deps (package foo))
  >  (action
  >   (with-stdout-to %{target}
  >    (progn
  >     (cat %{pkg:foo:share:payload}/a.txt)
  >     (cat %{pkg:foo:share:payload}/sub/b.txt)))))
  > EOF
  $ cp consumer-dune native/dune
  $ cp consumer-dune opaque/dune
  $ (cd native && make_named_package_project consumer 3.24 foo)
  $ (cd opaque && make_named_package_project consumer 3.24 foo)
  $ make_backend_lockdirs foo "$PWD/_provider"
  $ (cd native && dune build directory-files)
  $ cat native/_build/default/directory-files
  top
  nested
  $ test ! -e native/_build/default/.pkg/foo/_install/cookie

Opaque installation records leaf files, not the directory entry itself.
The native directory-pform spelling is therefore rejected for this backend.

  $ (cd opaque && dune build directory-files)
  File "dune", line 7, characters 9-33:
  7 |     (cat %{pkg:foo:share:payload}/a.txt)
               ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: File payload not found in section share of package foo
  File "dune", line 8, characters 9-33:
  8 |     (cat %{pkg:foo:share:payload}/sub/b.txt)))))
               ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: File payload not found in section share of package foo
  [1]

The same files can be consumed by naming the installed leaves directly.
This differs from the workspace descent errors above.

  $ cat > opaque/dune <<'EOF'
  > (rule
  >  (target directory-files)
  >  (deps (package foo))
  >  (action
  >   (with-stdout-to %{target}
  >    (progn
  >     (cat %{pkg:foo:share:payload/a.txt})
  >     (cat %{pkg:foo:share:payload/sub/b.txt})))))
  > EOF
  $ (cd opaque && dune build directory-files)
  $ cat opaque/_build/default/directory-files
  top
  nested
  $ cat opaque/_build/default/.pkg/foo/_install/opaque-recipe
  opaque
  $ test -f opaque/_build/default/.pkg/foo/_install/cookie
