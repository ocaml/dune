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

A selected project can be nested within its source snapshot. Its ordinary
private PPX belongs to that same project; no other package owns the driver.
Internal outputs must use the build partition, not the nested source root.

  $ mkdir -p _source/nested/driver
  $ cat > _source/nested/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name bar) (dir .))
  > EOF
  $ cat > _source/nested/dune <<'EOF'
  > (library (public_name bar) (preprocess (staged_pps identity)))
  > EOF
  $ echo 'let value = 3' > _source/nested/bar.ml
  $ cat > _source/nested/driver/dune <<'EOF'
  > (library
  >  (name identity)
  >  (kind ppx_rewriter)
  >  (ppx.driver (main Identity.main)))
  > EOF
  $ cat > _source/nested/driver/identity.ml <<'EOF'
  > let main () =
  >   let input = open_in_bin Sys.argv.(Array.length Sys.argv - 2) in
  >   let contents = really_input_string input (in_channel_length input) in
  >   close_in input;
  >   let output = open_out_bin Sys.argv.(Array.length Sys.argv - 1) in
  >   output_string output contents;
  >   close_out output
  > EOF
  $ (cd _source && dune build --root . --only-packages bar nested/bar.cma)
  $ make_lockpkg bar <<EOF
  > (version 1.0)
  > (source (copy $PWD/_source))
  > (build (run false))
  > EOF

  $ dune build _build/default/.lockfile/pkg/bar/nested/bar.cma
  $ test -f _build/default/.lockfile/pkg/bar/nested/bar.cma
  $ find _build/default/.lockfile/pkg/bar/.ppx -name ppx.exe | wc -l
  1
  $ test ! -e _build/default/.lockfile/pkg/bar/nested/.ppx

With distinct host and target contexts, both builds put the driver in the host
partition while compiling the consuming library in the target context.

  $ cat > _source/dune-workspace <<'EOF'
  > (lang dune 3.25)
  > (context (default))
  > (context (default (name host)))
  > (context (default (name target) (host host)))
  > EOF
  $ (cd _source && dune build --root . --only-packages bar _build/target/nested/bar.cma)
  $ find _source/_build/host/.ppx -name ppx.exe | wc -l
  1
  $ test ! -e _source/_build/target/.ppx
  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.25)
  > (pkg enabled)
  > (context (default))
  > (context (default (name host)))
  > (context (default (name target) (host host)))
  > EOF
  $ dune build _build/target/.lockfile/pkg/bar/nested/bar.cma
  $ test -f _build/target/.lockfile/pkg/bar/nested/bar.cma
  $ find _build/host/.lockfile/pkg/bar/.ppx -name ppx.exe | wc -l
  1
  $ test ! -e _build/target/.lockfile/pkg/bar/.ppx

A workspace does not need a context called default. Shared lock storage is not
itself a workspace build context.

  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.25)
  > (pkg enabled)
  > (context (default (name host)))
  > (context (default (name target) (host host)))
  > EOF

  $ echo 'let value = 4' > _source/nested/bar.ml
  $ dune build _build/target/.lockfile/pkg/bar/nested/bar.cma
  $ test -d _build/_private/default/.lock/dune.lock
  $ test -f _build/target/.lockfile/pkg/bar/nested/bar.cma
  $ find _build/host/.lockfile/pkg/bar/.ppx -name ppx.exe | wc -l
  1

Recursive aliases follow authored directory selection. They visit ordinary
children, but do not enter ignored or explicitly vendored directories.

  $ mkdir -p _source/checks/child _source/checks/ignored _source/checks/vendored
  $ cat > _source/checks/dune <<'EOF'
  > (dirs :standard \ ignored)
  > (vendored_dirs vendored)
  > EOF
  $ cat > _source/checks/child/dune <<'EOF'
  > (rule (alias scan) (action (echo scanned)))
  > EOF
  $ for dir in ignored vendored; do
  >   echo '(rule (alias scan) (action (run false)))' > _source/checks/$dir/dune
  > done
  $ cat >> _source/dune <<'EOF'
  > (rule
  >  (target traversed)
  >  (deps (alias_rec checks/scan))
  >  (action (write-file %{target} traversed)))
  > EOF
  $ (cd _source && dune build --root . --only-packages foo _build/host/traversed)
  scanned

  $ dune build _build/host/.lockfile/pkg/foo/traversed
  scanned
  $ cat _build/host/.lockfile/pkg/foo/traversed
  traversed

The same recursive alias can also be requested directly on the command line.

  $ (cd _source && dune build --root . --only-packages foo @_build/host/checks/scan)
  $ dune build @_build/host/.lockfile/pkg/foo/checks/scan

Implicit default aliases use the same source view and include ordinary targets.

  $ cat >> _source/checks/child/dune <<'EOF'
  > (rule (target implicit) (action (write-file %{target} implicit)))
  > EOF
  $ (cd _source && dune build --root . --only-packages foo @_build/host/checks/child/default)
  $ cat _source/_build/host/checks/child/implicit
  implicit
  $ dune build @_build/host/.lockfile/pkg/foo/checks/child/default
  $ cat _build/host/.lockfile/pkg/foo/checks/child/implicit
  implicit

Workspace alias traversal must not sweep in dependency mounts.

  $ dune build @_build/host/scan
  Error: Alias "scan" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  [1]

Requesting a source directory is shorthand for its default alias in either view.

  $ cat >> _source/checks/child/dune <<'EOF'
  > (rule (target direct) (action (write-file %{target} directory)))
  > EOF
  $ (cd _source && dune build --root . --only-packages foo _build/host/checks/child)
  $ cat _source/_build/host/checks/child/direct
  directory

BUG: directory target classification still checks only the workspace tree.

  $ dune build _build/host/.lockfile/pkg/foo/checks/child
  Error: Don't know how to build _build/host/.lockfile/pkg/foo/checks/child
  [1]
