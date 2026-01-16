Test for multiple libraries with inline_tests set in the same directory

  $ cat >dune-project <<EOF
  > (lang dune 2.6)
  > EOF

Create a dummy backend and two libraries with inline_tests

  $ cat >dune <<EOF
  > (library
  >  (name backend_simple)
  >  (modules ())
  >  (inline_tests.backend
  >   (generate_runner (echo "print_endline \"test\""))))
  > 
  > (library
  >  (name foo_simple1)
  >  (modules ())
  >  (inline_tests (backend backend_simple)))
  > 
  > (library
  >  (name foo_simple2)
  >  (modules ())
  >  (inline_tests (backend backend_simple)))
  > EOF

try to run them:

  $ env -u OCAMLRUNPARAM dune runtest
  test
  test
