  $ cat >dune-project <<EOF
  > (lang dune 2.6)
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name backend_tmb1)
  >  (modules ())
  >  (inline_tests.backend
  >   (generate_runner (echo foo))))
  > 
  > (library
  >  (name backend_tmb2)
  >  (modules ())
  >  (inline_tests.backend
  >   (generate_runner (echo foo))))
  > 
  > (library
  >  (name foo_tmb)
  >  (inline_tests)
  >  (libraries backend_tmb1 backend_tmb2))
  > EOF

  $ dune runtest
  File "dune", line 15, characters 1-15:
  15 |  (inline_tests)
        ^^^^^^^^^^^^^^
  Error: Too many independent inline tests backends found:
  - "backend_tmb1" in _build/default
  - "backend_tmb2" in _build/default
  [1]
