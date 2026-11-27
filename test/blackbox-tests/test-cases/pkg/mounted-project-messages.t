Errors about a mounted package's project name its directory by its build
path, since that is where its files are.

  $ make_lockdir
  $ make_named_package_project main 3.25 foo
  $ cat > dune <<'EOF'
  > (rule (alias use) (deps (package foo)) (action (echo used)))
  > EOF
  $ mkdir _foo
  $ make_dune_source_lockpkg native foo "$PWD/_foo"

A stanza that could belong to several packages lists their opam files.

  $ cat > _foo/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name foo))
  > (package (name bar))
  > EOF
  $ echo '(install (section share) (files data))' > _foo/dune
  $ touch _foo/data
  $ dune build @use 2>&1 | grep -v '^ *$'
  File "_build/default/.pkg/foo/dune", line 1, characters 0-38:
  Error: I can't determine automatically which package this stanza is for.
  I have the choice between these ones:
  - bar (because of _build/default/.pkg/foo/bar.opam)
  - foo (because of _build/default/.pkg/foo/foo.opam)
  You need to add a (package ...) field to this (install) stanza.
  [1]

A stanza naming a package in a project that defines none says where the
project was found.

  $ (cd _foo && make_dune_project_with_package 3.25 foo)
  $ echo '(library (public_name foo))' > _foo/dune
  $ mkdir _foo/nested
  $ echo '(lang dune 3.25)' > _foo/nested/dune-project
  $ echo '(install (section share) (files data) (package other))' > _foo/nested/dune
  $ dune build @use 2>&1 | grep -v '^ *$'
  File "_build/default/.pkg/foo/nested/dune", line 1, characters 47-52:
  Error: You cannot declare items to be installed without adding a
  <package>.opam file at the root of your project.
  To declare elements to be installed as part of package "other", add a
  "other.opam" file at the root of your project.
  Root of the project as discovered by dune: _build/default/.pkg/foo/nested
  [1]

Two packages cannot claim the same directory.

  $ rm -r _foo/nested
  $ cat > _foo/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name foo) (dir sub))
  > (package (name bar) (dir sub))
  > EOF
  $ mkdir _foo/sub
  $ echo '(library (public_name foo))' > _foo/sub/dune
  $ rm _foo/dune
  $ dune build @use 2>&1 | grep -v '^ *$'
  File "_build/default/.pkg/foo/dune-project", line 3, characters 25-28:
  Error: package _build/default/.pkg/foo/sub is already defined in "foo"
  [1]
