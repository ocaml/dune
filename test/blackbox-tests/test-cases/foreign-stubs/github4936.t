C stubs and the tests stanza

  $ touch e.ml stubs.c
  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > EOF
  $ cat > dune << EOF
  > (test
  >  (name e)
  >  (modes exe)
  >  (foreign_stubs
  >   (language c)
  >   (names stubs)))
  > EOF
  $ dune build
