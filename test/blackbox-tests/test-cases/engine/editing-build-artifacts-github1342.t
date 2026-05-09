Reproduction case for #1342. Check that when the user edits files in
_build, things are rebuild as expected.

Note that this is no longer supported. Users are not expected to edit things in
the _build directory.

This case now specifically checks that a source-backed build copy is repaired
when the corresponding build artifact is edited.

  $ echo 42 > x
  $ dune build x
  $ cat _build/default/x
  42

A source-backed file can replace a build-directory target directory. When that
happens, stale workspace-local cache entries for the old directory's descendants
must be invalidated.

  $ mkdir dir-to-file
  $ cd dir-to-file
  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (using directory-targets 0.1)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >  (targets (dir foo))
  >  (action (bash "mkdir foo; echo generated > foo/child")))
  > EOF
  $ dune build foo/child
  $ cat _build/default/foo/child
  generated
  $ echo > dune
  $ echo source > foo
  $ dune build foo
  $ cat _build/default/foo
  source
  $ rm foo
  $ cat > dune <<EOF
  > (rule
  >  (targets (dir foo))
  >  (action (bash "mkdir foo; echo generated > foo/child")))
  > EOF
  $ dune build foo/child
  $ cat _build/default/foo/child
  generated
  $ cd ..
  $ chmod u+w _build/default/x
  $ echo 0 > _build/default/x
  $ dune build x
  $ cat _build/default/x
  42
