OCaml's Unix module on Windows may not handle non-ASCII filenames correctly,
as many Windows APIs have both ANSI (A) and wide (W) variants. If readdir,
lstat, unlink, or rmdir use the ANSI variants, filenames with characters
outside the current code page will fail.

This test verifies that sandbox cleanup handles unicode filenames.

  $ trap 'cmd /c "rmdir /s /q _build" 2>/dev/null' EXIT

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > EOF

  $ cat > dune <<'EOF'
  > (rule
  >  (target output)
  >  (deps (sandbox always))
  >  (action (bash "mkdir données café 日本語 && echo ok > output")))
  > EOF

  $ dune build output 2>&1 | censor
  File "dune", lines 1-4, characters 0-118:
  1 | (rule
  2 |  (target output)
  3 |  (deps (sandbox always))
  4 |  (action (bash "mkdir données café 日本語 && echo ok > output")))
  Error: failed to delete sandbox in
  _build/.sandbox/$DIGEST
  Reason:
  rmdir(_build/.sandbox/$DIGEST\default): Directory not empty
  [1]
