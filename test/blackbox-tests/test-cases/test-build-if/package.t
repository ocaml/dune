build_if is compatible with package.

This is important to test because in that case, (test) can not be split into two stanzas:

  $ cat > dune-project << EOF
  > (lang dune 3.9)
  > 
  > (package (name a) (allow_empty))
  > EOF

  $ cat > dune << EOF
  > (test
  >  (name t)
  >  (package a)
  >  (build_if %{env:ENABLED=false}))
  > EOF

  $ touch t.ml

  $ dune runtest

If we try to split it we get an error:

  $ cat > dune << EOF
  > (executable
  >  (name t)
  >  (package a)
  >  (enabled_if %{env:ENABLED=false}))
  > 
  > (rule
  >  (alias runtest)
  >  (action (run ./t.exe))
  >  (package a)
  >  (enabled_if %{env:ENABLED=false}))
  > EOF

  $ dune runtest
  File "dune", line 3, characters 1-12:
  3 |  (package a)
       ^^^^^^^^^^^
  Error: This field is useless without a (public_name ...) field.
  [1]
