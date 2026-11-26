Mounted packages can produce build artifacts, but must never register or apply
promotions. Promotion controls on the command line must not override this.

  $ make_lockdir
  $ make_named_package_project main 3.25 foo
  $ mkdir _source
  $ (cd _source && make_dune_project_with_package 3.25 foo)
  $ echo authored > _source/source.txt
  $ echo workspace > expected
  $ cat > _source/dune <<'EOF'
  > (rule
  >  (target source.txt)
  >  (mode promote)
  >  (action (write-file %{target} generated)))
  > (rule
  >  (target escaped.txt)
  >  (mode (promote (into ../../)))
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
  >    (diff? ../../expected external.corrected)
  >    (write-file %{target} checked))))
  > EOF
  $ make_dune_source_lockpkg native foo "$PWD/_source"

Rule promotion produces only build artifacts, leaving the original inputs
untouched.

  $ dune build _build/default/.pkg/foo/source.txt
  $ cat _build/default/.pkg/foo/source.txt
  generated
  $ test ! -e _build/.to-delete-in-source-tree
  $ dune build --auto-promote _build/default/.pkg/foo/source.txt _build/default/.pkg/foo/escaped.txt
  $ cat _build/default/.pkg/foo/escaped.txt
  escaped
  $ test ! -e escaped.txt
  $ test ! -e .pkg

A missing diff operand is not permission to invent a workspace promotion path.
[check] builds, shows the correction and lists the registered promotions.

  $ check() { dune build "$@" 2>&1 | grep '^+corrected'; s=$?; dune promotion list; return $s; }
  $ check _build/default/.pkg/foo/checked
  +corrected
  [1]

A mounted action cannot promote into the workspace either.

  $ check _build/default/.pkg/foo/external-checked
  +corrected
  [1]

Neither manual nor automatic promotion can override these boundaries.

  $ dune promote
  $ check --auto-promote _build/default/.pkg/foo/checked
  +corrected
  [1]
  $ check --auto-promote _build/default/.pkg/foo/external-checked
  +corrected
  [1]
  $ test ! -e .pkg
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
  $ check --auto-promote _build/default/.pkg/foo/corrected-ok
  +corrected
  [1]
  $ dune build --auto-promote _build/default/.pkg/foo/patched-ok > patch-back-output 2>&1
  [1]
  $ grep -q 'should be deleted' patch-back-output
  $ grep -q 'new-file' patch-back-output
  $ grep -q '^+fresh' patch-back-output
  $ dune promotion list
  $ test ! -e .pkg

An alias-only action in the mount has no target to request, but its failing
diff must still be reported without registering or applying a promotion.

  $ echo authored > _source/alias-expected
  $ cat >> _source/dune <<'EOF'
  > (rule
  >  (alias mounted-check)
  >  (action
  >   (progn
  >    (write-file alias-expected.corrected corrected)
  >    (diff? alias-expected alias-expected.corrected))))
  > EOF
  $ check @_build/default/.pkg/foo/mounted-check
  +corrected
  [1]
  $ dune promote
  $ check --auto-promote @_build/default/.pkg/foo/mounted-check
  +corrected
  [1]
  $ test ! -e .pkg

Cram tests of a mounted package are never run, whether requested through the
mount or through the workspace.

  $ cat > _source/mounted.t <<EOF
  >   $ echo ran > $PWD/cram-ran
  > EOF
  $ dune build _build/default/.pkg/foo/mounted.t
  $ grep -c cram-ran _build/default/.pkg/foo/mounted.t
  1
  $ dune build @_build/default/.pkg/foo/runtest
  $ dune build @_build/default/.pkg/foo/mounted
  Error: Alias "mounted" specified on the command line is empty.
  It is not defined in .pkg/foo or any of its descendants.
  [1]
  $ dune build @runtest
  $ test ! -e cram-ran

A workspace rule in promote mode can copy a mounted target; the promotion
lands in the workspace.

  $ cat > dune <<'EOF'
  > (rule
  >  (target from-mount.txt)
  >  (mode promote)
  >  (action (copy .pkg/foo/source.txt %{target})))
  > EOF
  $ dune build from-mount.txt
  $ cat from-mount.txt
  generated
  $ test ! -e .pkg

The invariant is not tied to a context named default.

  $ make_host_target_workspace
  $ dune build --auto-promote _build/target/.pkg/foo/source.txt
  $ cat _build/target/.pkg/foo/source.txt
  generated
  $ check --auto-promote _build/host/.pkg/foo/checked
  +corrected
  [1]
  $ dune promote
  $ test ! -e .pkg
