Dune should not crash when `menhir_flags` are passed in a workspace file.
See #9024.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (using menhir 2.0)
  > EOF

  $ cat > dune-workspace << EOF
  > (lang dune 3.0)
  > (env
  >  (test (menhir_flags --table)))
  > EOF

  $ dune build 2>&1 | head -n 5
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Syntax identifier is unset",
    { name = "menhir"
    ; supported_versions =
