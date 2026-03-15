Nested cram tests are ignored unless a rule opts into corrections

  $ mkdir inner
  $ cat > inner/dune-project <<'EOF'
  > (lang dune 3.23)
  > EOF
  $ cat > inner/repro.t <<'EOF'
  >   $ echo existing > a.corrected
  > EOF
  $ dune runtest inner/repro.t && echo passed
  passed
  $ dune promotion list
