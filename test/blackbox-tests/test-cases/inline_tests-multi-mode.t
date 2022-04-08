Test running inline tests in multiple modes at once

Reproduction case for #3347

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name test)
  >  (libraries test_backend)
  >  (modules test)
  >  (inline_tests (modes byte native)))
  > 
  > (library
  >  (name test_backend)
  >  (modules ())
  >  (inline_tests.backend
  >   (generate_runner
  >    (progn
  >     (echo "Printf.eprintf \"Test %s\"\n")
  >     (echo "  (match Sys.backend_type with")
  >     (echo "   | Bytecode -> \"byte\"\n")
  >     (echo "   | Native -> \"native\"\n")
  >     (echo "   | Other s -> s)\n")))))
  > EOF

  $ touch test.ml

  $ dune runtest
  Test byte
  Test native
