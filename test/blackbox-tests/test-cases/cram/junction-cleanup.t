On Windows, MSYS2 may create junctions (directory reparse points) instead of
proper symbolic links. OCaml's Unix.lstat returns ENOENT for junctions whose
targets are unreachable, which causes them to be invisible to directory cleanup.

This test verifies that the cram test runner can clean up after a test that
creates junctions.

This is currently not the case.

  $ trap 'cmd /c "rmdir /s /q _build" 2>/dev/null' EXIT

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (cram enable)
  > EOF

Create a cram test that creates a junction inside its working directory:

  $ cat > junction.t <<'EOF'
  >   $ echo hi > file
  >   $ cmd /c "mklink /j junction file" > /dev/null
  > EOF

  $ dune runtest 2>&1 | strip_sandbox
  Error: failed to delete sandbox in
  $SANDBOX
  Reason:
  $SANDBOX\default): Directory not empty
  -> required by _build/default/.cram.junction.t/cram.out
  -> required by alias junction
  -> required by alias runtest
  [1]
