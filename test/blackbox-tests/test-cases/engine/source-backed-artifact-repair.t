A source-backed file can replace a build-directory target directory. When that
happens, stale workspace-local cache entries for the old directory's descendants
must be invalidated.

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
