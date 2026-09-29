The same authored project uses the same directory rules as a workspace project
and as a mounted dependency. Include-subdirs groups, source-only directories and
cram fixtures all retain their source view.

  $ make_lockdir
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (depends foo))
  > EOF
  $ mkdir -p _source/source-only _source/group/sub _source/fixture.t/empty
  $ cat > _source/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name foo) (dir .))
  > EOF
  $ cat > _source/dune <<'EOF'
  > (library (public_name foo))
  > (rule
  >  (target verified)
  >  (deps foo.cma group/group.cma
  >        (alias source-only/basic.t)
  >        (alias group/sub/group.t)
  >        (alias fixture.t))
  >  (action (write-file %{target} verified)))
  > EOF
  $ echo 'let value = 1' > _source/foo.ml
  $ cat > _source/group/dune <<'EOF'
  > (include_subdirs qualified)
  > (library (name group) (public_name foo.group))
  > EOF
  $ echo 'let value = 2' > _source/group/sub/value.ml
  $ cat > _source/source-only/basic.t <<'EOF'
  >   $ echo source-only
  >   source-only
  > EOF
  $ cat > _source/group/sub/group.t <<'EOF'
  >   $ echo group
  >   group
  > EOF
  $ cat > _source/fixture.t/run.t <<'EOF'
  >   $ cat payload
  >   fixture
  >   $ test -d empty
  > EOF
  $ echo fixture > _source/fixture.t/payload
  $ echo '(' > _source/fixture.t/dune-project

Run the project normally first. The malformed dune-project inside the cram
fixture is data, not project configuration.

  $ (cd _source && dune build --root . --only-packages foo verified)
  $ cat _source/_build/default/verified
  verified

Mount the identical source. The lock recipe is deliberately unusable, so the
same rules must be generated in the current graph. Workspace package selection
and inherited cram stanzas must not replace the mounted project's configuration.

  $ cat > dune <<'EOF'
  > (cram
  >  (applies_to :whole_subtree)
  >  (deps missing-workspace-only-dependency))
  > EOF

  $ make_lockpkg foo <<EOF
  > (version 1.0)
  > (source (copy $PWD/_source))
  > (build (run false))
  > EOF
  $ dune build --only-packages main _build/default/.lockfile/pkg/foo/verified
  $ cat _build/default/.lockfile/pkg/foo/verified
  verified
  $ test ! -e _build/default/.lockfile/pkg/foo/.opam/foo/target
