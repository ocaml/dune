enabled_if inside the inline_tests field in the library stanza

  $ mkdir tmp && cd tmp
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name backend_mbc)
  >  (modules ())
  >  (inline_tests.backend
  >   (generate_runner (echo "print_endline \"backend_mbc\""))))
  > 
  > (library
  >  (name foo_mbc)
  >  (inline_tests
  >   (enabled_if false)
  >   (backend backend_mbc))
  >  (libraries backend_mbc))
  > EOF

  $ dune runtest
