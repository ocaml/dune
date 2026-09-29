Mounted packages can produce build artifacts, but must never register or apply
promotions. Promotion controls on the command line must not override this.

  $ make_lockdir
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (depends foo))
  > EOF
  $ mkdir _source
  $ cat > _source/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name foo) (dir .))
  > EOF
  $ echo authored > _source/source.txt
  $ echo workspace > expected
  $ cat > _source/dune <<'EOF'
  > (rule
  >  (target source.txt)
  >  (mode promote)
  >  (action (write-file %{target} generated)))
  > (rule
  >  (target checked)
  >  (action
  >   (progn
  >    (write-file missing.corrected corrected)
  >    (diff? missing missing.corrected)
  >    (write-file %{target} checked))))
  > (rule
  >  (target external-checked)
  >  (action
  >   (progn
  >    (write-file external.corrected corrected)
  >    (diff? ../../../expected external.corrected)
  >    (write-file %{target} checked))))
  > EOF
  $ make_lockpkg foo <<EOF
  > (version 1.0)
  > (source (copy $PWD/_source))
  > (build (run false))
  > EOF

Rule promotion produces only build artifacts, leaving the original and fetched
inputs untouched.

  $ dune build _build/default/.lockfile/pkg/foo/source.txt
  $ cat _build/default/.lockfile/pkg/foo/source.txt
  generated
  $ cat _source/source.txt
  authored
  $ find _build/_fetch -path '*/dir/source.txt' -exec cat '{}' \;
  authored
  $ test ! -e .lockfile
  $ test ! -e _build/.to-delete-in-source-tree

A missing diff operand is not permission to invent a workspace promotion path.
BUG: the current implementation nevertheless registers these promotions.

  $ dune build _build/default/.lockfile/pkg/foo/checked > diff-output 2>&1
  [1]
  $ grep '^+corrected' diff-output
  +corrected
  $ dune promotion list
  .lockfile/pkg/foo/missing
  $ test ! -e _build/.to-promote
  [1]

A mounted action cannot promote into the workspace either.

  $ dune build _build/default/.lockfile/pkg/foo/external-checked > external-diff-output 2>&1
  [1]
  $ grep '^+corrected' external-diff-output
  +corrected
  $ dune promotion list
  expected
  $ test ! -e _build/.to-promote
  [1]

The same restriction applies when the requesting action is in the workspace.

  $ cat > dune <<'EOF'
  > (rule
  >  (target checked)
  >  (action
  >   (progn
  >    (write-file missing.corrected corrected)
  >    (diff? .lockfile/pkg/foo/workspace-missing missing.corrected)
  >    (write-file %{target} checked))))
  > EOF
  $ dune build checked > workspace-diff-output 2>&1
  [1]
  $ grep '^+corrected' workspace-diff-output
  +corrected
  $ dune promotion list
  .lockfile/pkg/foo/workspace-missing
  $ test ! -e _build/.to-promote
  [1]
