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
  >  (target escaped.txt)
  >  (mode (promote (into ../../../)))
  >  (action (write-file %{target} escaped)))
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
  $ dune build --auto-promote _build/default/.lockfile/pkg/foo/source.txt _build/default/.lockfile/pkg/foo/escaped.txt
  $ cat _build/default/.lockfile/pkg/foo/escaped.txt
  escaped
  $ test ! -e escaped.txt
  $ test ! -e .lockfile
  $ cat _source/source.txt
  authored

A missing diff operand is not permission to invent a workspace promotion path.

  $ dune build _build/default/.lockfile/pkg/foo/checked > diff-output 2>&1
  [1]
  $ grep '^+corrected' diff-output
  +corrected
  $ dune promotion list
  $ test ! -e _build/.to-promote

A mounted action cannot promote into the workspace either.

  $ dune build _build/default/.lockfile/pkg/foo/external-checked > external-diff-output 2>&1
  [1]
  $ grep '^+corrected' external-diff-output
  +corrected
  $ dune promotion list
  $ test ! -e _build/.to-promote

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
  $ test ! -e _build/.to-promote

Neither manual nor automatic promotion can override these boundaries.

  $ dune promote
  $ dune build --auto-promote _build/default/.lockfile/pkg/foo/checked > auto-diff-output 2>&1
  [1]
  $ dune build --auto-promote _build/default/.lockfile/pkg/foo/external-checked > auto-external-output 2>&1
  [1]
  $ dune build --auto-promote checked > auto-workspace-output 2>&1
  [1]
  $ dune promotion list
  $ test ! -e _build/.to-promote
  $ test ! -e .lockfile
  $ cat expected
  workspace

Sandbox corrections and patch-back changes report failures without registering
promotions, including file deletion and creation.

  $ echo original > _source/check.txt
  $ cat >> _source/dune <<'EOF'
  > (rule
  >  (target corrected-ok)
  >  (corrections produce)
  >  (deps check.txt)
  >  (action
  >   (progn
  >    (system "echo corrected > check.txt.corrected")
  >    (write-file %{target} done))))
  > (rule
  >  (target patched-ok)
  >  (deps check.txt (sandbox patch_back_source_tree))
  >  (action
  >   (progn
  >    (system "rm check.txt; echo fresh > new-file")
  >    (write-file %{target} done))))
  > EOF
  $ dune build --auto-promote _build/default/.lockfile/pkg/foo/corrected-ok > correction-output 2>&1
  [1]
  $ grep '^+corrected' correction-output
  +corrected
  $ dune promotion list
  $ test ! -e _build/.to-promote
  $ dune build --auto-promote _build/default/.lockfile/pkg/foo/patched-ok > patch-back-output 2>&1
  [1]
  $ grep -q 'should be deleted' patch-back-output
  $ dune promotion list
  $ test ! -e _build/.to-promote
  $ test ! -e .lockfile
  $ cat _source/check.txt
  original

Cram failures and timeout corrections are also non-promotable.

  $ mkdir _source/cram
  $ cat > _source/cram/dune <<'EOF'
  > (cram (timeout 1))
  > EOF
  $ cat > _source/cram/mismatch.t <<'EOF'
  >   $ echo actual
  >   expected
  > EOF
  $ dune runtest --auto-promote _build/default/.lockfile/pkg/foo/cram/mismatch.t > cram-output 2>&1
  [1]
  $ grep '^+  actual' cram-output
  +  actual
  $ dune promotion list
  $ test ! -e _build/.to-promote
  $ cat > _source/cram/timeout.t <<'EOF'
  >   $ echo actual
  >   expected
  >   $ sleep 5
  > EOF
  $ dune runtest --auto-promote _build/default/.lockfile/pkg/foo/cram/timeout.t > timeout-output 2>&1
  [1]
  $ grep -q 'timed out' timeout-output
  $ dune promotion list
  $ test ! -e _build/.to-promote
  $ test ! -e .lockfile
  $ grep expected _source/cram/mismatch.t _source/cram/timeout.t
  _source/cram/mismatch.t:  expected
  _source/cram/timeout.t:  expected

Ordinary workspace promotion remains available.

  $ cat > dune <<'EOF'
  > (rule
  >  (alias correct)
  >  (action
  >   (progn
  >    (write-file expected.corrected corrected)
  >    (diff? expected expected.corrected))))
  > EOF
  $ dune build @correct > ordinary-output 2>&1
  [1]
  $ dune promotion list
  expected
  $ dune promote
  Promoting _build/default/expected.corrected to expected.
  $ cat expected
  corrected

Workspace rule promotion cannot use into to enter reserved source namespaces or
the physical build directory.

  $ cat >> dune <<'EOF'
  > (rule
  >  (target logical-escape.txt)
  >  (mode (promote (into .lockfile/pkg/foo)))
  >  (action (write-file %{target} logical)))
  > (rule
  >  (target physical-escape.txt)
  >  (mode (promote (into _build/default/.lockfile/pkg/foo)))
  >  (action (write-file %{target} physical)))
  > EOF
  $ dune build logical-escape.txt physical-escape.txt
  $ cat _build/default/logical-escape.txt
  logical
  $ cat _build/default/physical-escape.txt
  physical
  $ test ! -e .lockfile
  $ test ! -e _build/default/.lockfile/pkg/foo/physical-escape.txt

Each produced target is checked as well as the destination directory. A target
named after the build directory must remain an ordinary artifact.

  $ cat >> dune <<'EOF'
  > (rule
  >  (target _build)
  >  (mode promote)
  >  (action (write-file %{target} artifact)))
  > EOF
  $ dune build _build/default/_build
  $ cat _build/default/_build
  artifact
  $ test -d _build

The invariant is not tied to a context named default.

  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.25)
  > (pkg enabled)
  > (context (default (name host)))
  > (context (default (name target) (host host)))
  > EOF
  $ dune build --auto-promote _build/target/.lockfile/pkg/foo/source.txt
  $ cat _build/target/.lockfile/pkg/foo/source.txt
  generated
  $ dune build --auto-promote _build/host/.lockfile/pkg/foo/checked > host-output 2>&1
  [1]
  $ dune promotion list
  $ dune promote
  $ test ! -e .lockfile
  $ cat _source/source.txt
  authored

Ordinary artifact generation must not depend on unrelated promotion namespaces.
Adding an unused context does not rerun an unchanged action in the host context.

  $ cat >> dune <<'EOF'
  > (rule
  >  (target ordinary)
  >  (action (progn (echo executed) (write-file %{target} ordinary))))
  > EOF
  $ dune build _build/host/ordinary
  executed
  $ dune build _build/host/ordinary
  $ echo '(context (default (name unused)))' >> dune-workspace
  $ dune build _build/host/ordinary
  $ dune promotion list
  $ test ! -e .lockfile
